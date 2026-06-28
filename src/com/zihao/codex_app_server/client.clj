(ns com.zihao.codex-app-server.client
  "Service-owned Codex app-server JSON-RPC client registry."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.zihao.agent-common.interface :as log]
            [com.zihao.codex-app-server.codec :as codec]
            [com.zihao.codex-app-server.process :as process]))

(defrecord Service [config clients*])

(def ^:private turn-diagnostic-max-events 120)

(defn start!
  [config]
  (->Service (or config {}) (atom {})))

(defn close!
  [service]
  (doseq [[_ client] @(:clients* service)]
    (process/destroy-client! client))
  (reset! (:clients* service) {})
  nil)

(defn- now-ms []
  (System/currentTimeMillis))

(defn- now-iso []
  (str (java.time.Instant/now)))

(defn- json-line
  [x]
  (json/write-str x))

(defn- bounded-conj
  [v x limit]
  (let [v* (conj (vec (or v [])) x)
        n (count v*)]
    (if (> n limit)
      (vec (subvec v* (- n limit)))
      v*)))

(defn- text-preview
  [s max-len]
  (let [s* (str (or s ""))]
    (if (<= (count s*) max-len)
      s*
      (str (subs s* 0 max-len) "..."))))

(defn- summarize-wire-msg
  [msg]
  (let [method (:method msg)
        item (get-in msg [:params :item])]
    (cond-> {:method method}
      (= "item/completed" method)
      (merge {:item-type (:type item)
              :item-id (:id item)
              :status (:status item)}
             (when (= "agentMessage" (:type item))
               {:agent-text-len (count (str (or (:text item) "")))}))

      (= "turn/completed" method)
      (assoc :turn-status (get-in msg [:params :turn :status]))

      (= "codex/event/task_complete" method)
      (assoc :has-last-agent-message
             (boolean (or (get-in msg [:params :last_agent_message])
                          (get-in msg [:params :lastAgentMessage])))))))

(defn- task-complete-last-agent-message
  [msg]
  (when (= "codex/event/task_complete" (:method msg))
    (or (get-in msg [:params :last_agent_message])
        (get-in msg [:params :lastAgentMessage]))))

(defn- append-turn-diagnostic!
  [entry]
  (try
    (let [dir (io/file "tmp" "codex-app-server-diagnostics")
          _ (.mkdirs dir)
          f (io/file dir "turn-empty-final.jsonl")]
      (spit f (str (json-line entry) "\n") :append true))
    (catch Exception e
      (log/warn (str "[CodexAppServer][diag] write failed: " (.getMessage e))))))

(defn send-json!
  [writer payload]
  (.write ^java.io.Writer writer (json-line payload))
  (.write ^java.io.Writer writer "\n")
  (.flush ^java.io.Writer writer))

(defn- next-id!
  [id-atom]
  (swap! id-atom inc))

(defn- request-config
  [service request]
  (merge (:config service)
         (select-keys request [:codex-home :model])))

(defn- client-cwd
  [request]
  (or (:workdir-path request)
      (.getAbsolutePath (io/file "."))))

(defn- normalize-dynamic-tool-spec
  [spec]
  (let [name (some-> (:name spec) str/trim)
        description (some-> (:description spec) str/trim)
        input-schema (or (:input-schema spec) (:inputSchema spec))]
    (when (str/blank? name)
      (throw (ex-info "dynamic-tools item requires non-empty :name"
                      {:type :user-error
                       :tool spec})))
    (when (str/blank? description)
      (throw (ex-info "dynamic-tools item requires non-empty :description"
                      {:type :user-error
                       :tool spec})))
    (when-not (map? input-schema)
      (throw (ex-info "dynamic-tools item requires map :input-schema"
                      {:type :user-error
                       :tool spec})))
    {:name name
     :description description
     :inputSchema input-schema}))

(defn normalize-dynamic-tools
  [request]
  (let [tools (:dynamic-tools request)]
    (cond
      (nil? tools) nil
      (and (sequential? tools) (empty? tools)) nil
      (sequential? tools)
      (->> tools
           (map normalize-dynamic-tool-spec)
           vec)
      :else
      (throw (ex-info "dynamic-tools must be a sequential collection"
                      {:type :user-error
                       :dynamic-tools tools})))))

(defn desired-experimental-api?
  [request]
  (let [dynamic-tools (normalize-dynamic-tools request)
        explicit-flag (:experimental-api? request)]
    (when (and (seq dynamic-tools)
               (false? explicit-flag))
      (throw (ex-info "dynamic-tools requires :experimental-api? true"
                      {:type :user-error})))
    (boolean (or explicit-flag (seq dynamic-tools)))))

(defn build-thread-start-params
  [config request]
  (let [cwd (client-cwd request)
        model (or (:model request) (:model config) "gpt-5-codex")
        dynamic-tools (normalize-dynamic-tools request)]
    (cond-> {:cwd cwd
             :approvalPolicy "never"
             :sandbox "danger-full-access"
             :model model}
      (seq dynamic-tools) (assoc :dynamicTools dynamic-tools))))

(defn- clear-cancel-fn!
  [cancel-fn-atom]
  (when (instance? clojure.lang.IAtom cancel-fn-atom)
    (reset! cancel-fn-atom nil)))

(defn- make-client!
  [service session-key request]
  (let [config (request-config service request)
        codex-home (process/resolve-codex-home config)
        process (process/start-codex-app-server-process! config codex-home)
        reader (java.io.BufferedReader. (io/reader (.getInputStream process)))
        writer (io/writer (.getOutputStream process))
        stdout-stream (.getInputStream process)
        _stderr-pump (process/start-stderr-pump! process)]
    {:session-key session-key
     :process process
     :reader reader
     :writer writer
     :stdout-stream stdout-stream
     :thread-id nil
     :turn-count 0
     :id-atom (atom 0)
     :lock (Object.)
     :created-at-ms (now-ms)
     :last-cwd (client-cwd request)
     :codex-home codex-home
     :experimental-api? nil
     :dynamic-tools nil}))

(defn- desired-thread-id
  [request]
  (some-> (:codex-thread-id request) str str/trim not-empty))

(defn- ensure-client!
  [service session-key request]
  (let [clients* (:clients* service)
        current (get @clients* session-key)
        desired-exp? (desired-experimental-api? request)
        desired-codex-home (process/resolve-codex-home (request-config service request))
        desired-thread (desired-thread-id request)
        reusable? (and current
                       (.isAlive ^Process (:process current))
                       (= desired-codex-home (:codex-home current))
                       (or (not (:initialized? current))
                           (= desired-exp? (boolean (:experimental-api? current))))
                       (or (nil? desired-thread)
                           (nil? (:thread-id current))
                           (= desired-thread (:thread-id current))))]
    (if reusable?
      current
      (do
        (when current (process/destroy-client! current))
        (let [fresh (make-client! service session-key request)]
          (swap! clients* assoc session-key fresh)
          fresh)))))

(defn read-next-msg!
  [client]
  (let [line (.readLine ^java.io.BufferedReader (:reader client))]
    (when line
      (codec/parse-json-safe line))))

(defn- wait-for-response!
  [client req-id]
  (loop []
    (let [msg (read-next-msg! client)]
      (when-not msg
        (throw (ex-info "app-server stream closed while waiting for response"
                        {:request-id req-id})))
      (cond
        (= req-id (:id msg))
        (if-let [err (:error msg)]
          (throw (ex-info "app-server request failed"
                          {:request-id req-id
                           :error err}))
          (:result msg))

        :else
        (recur)))))

(defn send-request!
  [client method params]
  (let [id (next-id! (:id-atom client))]
    (send-json! (:writer client) {:id id :method method :params params})
    (wait-for-response! client id)))

(defn- thread-name-set-params
  [thread-id thread-name]
  {:threadId thread-id
   :threadName thread-name})

(defn- nonblank-string?
  [x]
  (not (str/blank? (str (or x "")))))

(defn- callback
  [request k]
  (get-in request [:callbacks k]))

(defn- emit-event!
  [request event]
  (when-let [f (callback request :on-event)]
    (f event)))

(defn- notify-thread-started!
  [request thread-id thread-title]
  (let [event (cond-> {:codex-thread-id thread-id}
                (nonblank-string? thread-title)
                (assoc :codex-thread-title (str/trim (str thread-title))))]
    (when-let [f (callback request :on-thread-started)]
      (f event))
    (emit-event! request (assoc event :type :codex-app-server/thread-started))))

(defn- notify-thread-title-updated!
  [request thread-id thread-title]
  (when (nonblank-string? thread-title)
    (let [event {:codex-thread-id thread-id
                 :codex-thread-title (str/trim (str thread-title))}]
      (when-let [f (callback request :on-thread-title-updated)]
        (f event))
      (emit-event! request (assoc event :type :codex-app-server/thread-title-updated)))))

(defn- notify-turn-started!
  [request thread-id turn-id]
  (if (str/blank? (str (or turn-id "")))
    (let [event {:codex-thread-id thread-id
                 :reason :missing-turn-id}]
      (when-let [f (callback request :on-turn-running-unknown)]
        (f event))
      (emit-event! request (assoc event :type :codex-app-server/turn-running-unknown)))
    (let [event {:codex-thread-id thread-id
                 :codex-turn-id turn-id}]
      (when-let [f (callback request :on-turn-started)]
        (f event))
      (emit-event! request (assoc event :type :codex-app-server/turn-started)))))

(defn- ensure-protocol-initialized!
  [service client request]
  (let [clients* (:clients* service)
        dynamic-tools (normalize-dynamic-tools request)
        desired-exp? (desired-experimental-api? request)]
    (when-not (:initialized? client)
      (send-request! client
                     "initialize"
                     {:clientInfo {:name "synapse_codex_app_server"
                                   :title "Synapse Codex app-server"
                                   :version "0.1.0"}
                      :capabilities {:experimentalApi desired-exp?}})
      (send-json! (:writer client) {:method "initialized" :params {}})
      (send-request! client "account/read" {:refreshToken false})
      (swap! clients* update (:session-key client)
             assoc
             :initialized? true
             :experimental-api? desired-exp?
             :dynamic-tools dynamic-tools))
    (get @clients* (:session-key client))))

(defn- ensure-thread!
  [service client request]
  (let [clients* (:clients* service)
        config (request-config service request)
        dynamic-tools (normalize-dynamic-tools request)
        desired-exp? (desired-experimental-api? request)
        desired-remote-thread-id (desired-thread-id request)]
    (when (and (:thread-id client)
               (seq dynamic-tools)
               (not= dynamic-tools (:dynamic-tools client)))
      (throw (ex-info "dynamic-tools can only be set when starting a new thread"
                      {:type :user-error
                       :thread-id (:thread-id client)
                       :hint "Use a new session key for a fresh app-server thread."})))
    (let [client (ensure-protocol-initialized! service client request)]
      (cond
        (some? (:thread-id client))
        client

        (some? desired-remote-thread-id)
        (do
          (swap! clients* update (:session-key client)
                 assoc
                 :initialized? true
                 :thread-id desired-remote-thread-id
                 :experimental-api? desired-exp?
                 :dynamic-tools dynamic-tools)
          (get @clients* (:session-key client)))

        :else
        (let [thread-rsp (send-request! client
                                        "thread/start"
                                        (build-thread-start-params config request))
              remote-thread (:thread thread-rsp)
              thread-id (:id remote-thread)
              thread-title (codec/thread-display-name remote-thread)
              desired-thread-title (:thread-title request)]
          (when (str/blank? (str thread-id))
            (throw (ex-info "thread/start returned blank thread id" {})))
          (notify-thread-started! request thread-id thread-title)
          (when (and (string? desired-thread-title)
                     (not= desired-thread-title thread-title))
            (try
              (send-request! client
                             "thread/name/set"
                             (thread-name-set-params thread-id desired-thread-title))
              (catch Exception e
                (log/warn (str "[CodexAppServer] initial thread/name/set failed: "
                               (.getMessage e))))))
          (swap! clients* update (:session-key client)
                 (fn [c]
                   (assoc c
                          :initialized? true
                          :thread-id thread-id
                          :turn-count 0
                          :experimental-api? desired-exp?
                          :dynamic-tools dynamic-tools)))
          (get @clients* (:session-key client)))))))

(defn rollback-turns-needed
  [current-turn-count desired-turn-count]
  (let [current (max 0 (long (or current-turn-count 0)))
        desired (max 0 (long (or desired-turn-count 0)))]
    (when (< desired current)
      (- current desired))))

(defn- align-thread-turn-count!
  [service client desired-turn-count]
  (if (nil? desired-turn-count)
    client
    (let [clients* (:clients* service)
          desired (max 0 (long (or desired-turn-count 0)))
          rollback-turns (rollback-turns-needed (:turn-count client) desired)]
      (cond
        (zero? (long (or rollback-turns 0)))
        client

        (some? rollback-turns)
        (do
          (send-request! client
                         "thread/rollback"
                         {:threadId (:thread-id client)
                          :numTurns rollback-turns})
          (swap! clients* update (:session-key client) assoc :turn-count desired)
          (get @clients* (:session-key client)))

        :else
        client))))

(defn- best-effort-interrupt!
  [client turn-id]
  (try
    (when (and client (string? turn-id))
      (send-request! client "turn/interrupt" {:threadId (:thread-id client)
                                              :turnId turn-id}))
    (catch Exception e
      (log/debug (str "[CodexAppServer] interrupt failed: " (.getMessage e)))))
  nil)

(defn- dynamic-tool-error-result
  [message-text]
  {:success false
   :content-items [{:type "inputText"
                    :text message-text}]})

(defn- normalize-dynamic-tool-handler-result
  [raw-result]
  (cond
    (map? raw-result)
    {:success (boolean (:success raw-result))
     :content-items (or (:content-items raw-result) [])}

    (string? raw-result)
    {:success true
     :content-items [{:type "inputText" :text raw-result}]}

    :else
    (dynamic-tool-error-result "dynamic tool handler returned unsupported response shape")))

(defn- handle-dynamic-tool-call!
  [client on-dynamic-tool-call request]
  (let [{:keys [request-id] :as call} request
        raw-result (try
                     (if on-dynamic-tool-call
                       (on-dynamic-tool-call (dissoc call :request-id))
                       (dynamic-tool-error-result
                        (str "No dynamic tool handler configured for " (:tool call))))
                     (catch Throwable e
                       (dynamic-tool-error-result
                        (str "dynamic tool handler failed: " (.getMessage e)))))
        response (codec/dynamic-tool-call-response request-id
                                                   (normalize-dynamic-tool-handler-result raw-result))]
    (send-json! (:writer client) response)))

(defn- require-input-items
  [request]
  (let [input-items (:input-items request)]
    (when-not (seq input-items)
      (throw (ex-info "Missing input-items for app-server turn/start" {})))
    (vec input-items)))

(defn- run-turn-on-client!
  [service client request]
  (let [input-items (require-input-items request)
        turn-rsp (send-request! client
                                "turn/start"
                                {:threadId (:thread-id client)
                                 :input input-items})
        turn-id (get-in turn-rsp [:turn :id])
        clients* (:clients* service)
        _ (swap! clients* update (:session-key client) assoc :active-turn-id turn-id)
        _ (notify-turn-started! request (:thread-id client) turn-id)
        on-delta (callback request :on-delta)
        on-thinking-delta (callback request :on-thinking-delta)
        on-agent-message-started (callback request :on-agent-message-started)
        on-agent-message-delta (callback request :on-agent-message-delta)
        on-agent-message-completed (callback request :on-agent-message-completed)
        on-item-started (callback request :on-item-started)
        on-item-completed (callback request :on-item-completed)
        on-command-delta (callback request :on-command-delta)
        on-dynamic-tool-call (callback request :on-dynamic-tool-call)
        stream-atom (:stream-atom request)
        cancel-fn-atom (:cancel-fn-atom request)
        final-text (atom nil)
        final-agent-message-item-id (atom nil)
        latest-nonblank-agent-text (atom nil)
        latest-agent-message-item-id (atom nil)
        usage-atom (atom nil)
        terminal-error (atom nil)
        item-completed-agent-texts (atom [])
        task-complete-agent-text (atom nil)
        agent-delta-chars (atom 0)
        reasoning-delta-chars (atom 0)
        recent-wire-events (atom [])]
    (when (instance? clojure.lang.IAtom stream-atom)
      (reset! stream-atom (:stdout-stream client)))
    (when (instance? clojure.lang.IAtom cancel-fn-atom)
      (reset! cancel-fn-atom (fn [] (best-effort-interrupt! client turn-id))))
    (try
      (loop []
        (when (Thread/interrupted)
          (best-effort-interrupt! client turn-id)
          (throw (InterruptedException. "Codex app-server turn interrupted")))
        (let [msg (read-next-msg! client)]
          (when-not msg
            (throw (ex-info "app-server stream closed during turn"
                            {:turn-id turn-id})))
          (swap! recent-wire-events bounded-conj (summarize-wire-msg msg) turn-diagnostic-max-events)
          (emit-event! request {:type :codex-app-server/wire-message
                                :method (:method msg)})
          (log/debug (str "[CodexAppServer][wire] method=" (:method msg)
                          " keys=" (pr-str (keys msg))
                          (when (#{"item/started" "item/completed"} (:method msg))
                            (str " params=" (pr-str (:params msg))))))
          (when-let [{:keys [thread-id thread-name]} (codec/thread-name-updated msg)]
            (when (or (str/blank? (str (or thread-id "")))
                      (= thread-id (:thread-id client)))
              (notify-thread-title-updated! request
                                            (or thread-id (:thread-id client))
                                            thread-name)))
          (if-let [tool-call-req (codec/dynamic-tool-call-request msg)]
            (do
              (handle-dynamic-tool-call! client on-dynamic-tool-call tool-call-req)
              (recur))
            (do
              (when-let [{:keys [item-id delta] :as delta-data} (codec/agent-message-delta msg)]
                (reset! latest-agent-message-item-id item-id)
                (swap! agent-delta-chars + (count (str delta)))
                (if on-agent-message-delta
                  (on-agent-message-delta delta-data)
                  (when on-delta (on-delta delta))))
              (when-let [d (codec/reasoning-delta-text msg)]
                (swap! reasoning-delta-chars + (count (str d)))
                (when on-thinking-delta (on-thinking-delta d)))
              (when-let [{:keys [item-id text] :as agent-message} (codec/completed-agent-message msg)]
                (reset! latest-agent-message-item-id item-id)
                (swap! item-completed-agent-texts bounded-conj text 8)
                (when on-agent-message-completed
                  (on-agent-message-completed agent-message))
                (when-not (str/blank? (str text))
                  (reset! final-text text)
                  (reset! final-agent-message-item-id item-id)
                  (reset! latest-nonblank-agent-text text)))
              (when-let [task-last-msg (task-complete-last-agent-message msg)]
                (reset! task-complete-agent-text task-last-msg))
              (when-let [u (codec/usage-from-thread-token-updated msg)]
                (reset! usage-atom u))
              (when-let [err (codec/terminal-error-for-turn msg turn-id)]
                (reset! terminal-error err))
              (when-let [started (codec/item-started msg)]
                (let [item-data (:data started)]
                  (if (= "agentMessage" (:item-type started))
                    (do
                      (reset! latest-agent-message-item-id (:item-id started))
                      (when on-agent-message-started
                        (on-agent-message-started
                         {:item-id (:item-id started)
                          :text (:text item-data)})))
                    (when on-item-started
                      (on-item-started
                       (merge {:item-type (:item-type started)
                               :item-id (:item-id started)}
                              (when (= "commandExecution" (:item-type started))
                                {:command (codec/item-started-command item-data)})
                              (when (= "fileChange" (:item-type started))
                                (codec/item-file-change-info item-data))
                              (codec/item-started-dynamic-tool-info item-data)
                              (codec/item-image-generation-info item-data)))))))
              (when-let [d (codec/command-output-delta msg)]
                (when on-command-delta (on-command-delta d)))
              (when-let [_d (codec/file-change-output-delta msg)]
                nil)
              (when-let [completed (codec/item-completed msg)]
                (when (not= "agentMessage" (:item-type completed))
                  (let [item-data (:data completed)
                        cmd-info (codec/item-completed-command-info item-data)
                        fc-info (codec/item-completed-file-change-info item-data)
                        dt-info (codec/item-completed-dynamic-tool-info item-data)
                        image-info (codec/item-image-generation-info item-data)]
                    (when on-item-completed
                      (on-item-completed
                       (merge {:item-type (:item-type completed)
                               :item-id (:item-id completed)
                               :status (:status completed)}
                              (when cmd-info cmd-info)
                              (when fc-info fc-info)
                              (when dt-info dt-info)
                              (when image-info image-info)))))))
              (if (codec/turn-completed? msg turn-id)
                (let [status (codec/turn-completed-status msg)
                      msg-error (or @terminal-error (codec/turn-completed-error-message msg))
                      task-last (str (or @task-complete-agent-text ""))
                      text (cond
                             (not (str/blank? (str @final-text)))
                             (str @final-text)

                             (not (str/blank? task-last))
                             task-last

                             (not (str/blank? (str @latest-nonblank-agent-text)))
                             (str @latest-nonblank-agent-text)

                             :else
                             "")
                      agent-message-item-id (or @final-agent-message-item-id
                                                @latest-agent-message-item-id)]
                  (when (and (= status "completed")
                             (str/blank? text))
                    (let [observed-texts (vec @item-completed-agent-texts)]
                      (append-turn-diagnostic!
                       {:timestamp (now-iso)
                        :event "turn_completed_empty_final_text"
                        :thread-id (:thread-id client)
                        :turn-id turn-id
                        :status status
                        :final-text-len (count text)
                        :item-completed-agent-text-count (count observed-texts)
                        :item-completed-agent-text-lens (mapv #(count (str %)) observed-texts)
                        :item-completed-agent-previews (mapv #(text-preview % 280) observed-texts)
                        :task-complete-last-agent-message-len (count task-last)
                        :task-complete-last-agent-message-preview (text-preview task-last 280)
                        :agent-delta-chars @agent-delta-chars
                        :reasoning-delta-chars @reasoning-delta-chars
                        :recent-wire-events @recent-wire-events})
                      (log/warn (str "[CodexAppServer][diag] empty final text on completed turn, "
                                     "thread-id=" (:thread-id client) ", turn-id=" turn-id
                                     ". Snapshot appended to tmp/codex-app-server-diagnostics/turn-empty-final.jsonl"))))
                  (case status
                    "completed" {:status :completed
                                 :text text
                                 :usage @usage-atom
                                 :agent-message-item-id agent-message-item-id}
                    "interrupted" {:status :interrupted
                                   :text text
                                   :usage @usage-atom
                                   :agent-message-item-id agent-message-item-id}
                    {:status :failed
                     :text text
                     :usage @usage-atom
                     :error-message msg-error
                     :turn-id turn-id}))
                (recur))))))
      (finally
        (swap! clients* update (:session-key client) dissoc :active-turn-id)
        (clear-cancel-fn! cancel-fn-atom)
        (when (instance? clojure.lang.IAtom stream-atom)
          (reset! stream-atom nil))))))

(defn- require-session-key
  [request]
  (or (some-> (:session-key request) str str/trim not-empty)
      (throw (ex-info "session-key required for Codex app-server request"
                      {:type :user-error}))))

(defn run-turn!
  [service request]
  (when (Thread/interrupted)
    (throw (InterruptedException. "Codex app-server turn interrupted")))
  (let [key (require-session-key request)
        client (ensure-client! service key request)
        clients* (:clients* service)]
    (try
      #_:clj-kondo/ignore
      (locking (:lock client)
        (let [client* (ensure-thread! service client request)
              client** (align-thread-turn-count! service client* (:desired-turn-count request))
              result (run-turn-on-client! service client** request)]
          (swap! clients* update key
                 (fn [c]
                   (if c
                     (update c :turn-count (fnil inc 0))
                     c)))
          (assoc result :codex-thread-id (:thread-id client**))))
      (catch InterruptedException e
        (when-let [c (get @clients* key)]
          (process/destroy-client! c))
        (swap! clients* dissoc key)
        (throw e))
      (catch clojure.lang.ExceptionInfo e
        (let [m (str (.getMessage e))]
          (when (or (str/includes? m "stream closed")
                    (str/includes? m "request failed"))
            (when-let [c (get @clients* key)]
              (process/destroy-client! c))
            (swap! clients* dissoc key)))
        (throw e))
      (catch Exception e
        (when-let [c (get @clients* key)]
          (process/destroy-client! c))
        (swap! clients* dissoc key)
        (throw e)))))

(defn read-thread
  [service request]
  (let [codex-thread-id (or (desired-thread-id request)
                            (throw (ex-info "codex-thread-id required for thread/read"
                                            {:blame :caller})))
        key (require-session-key request)
        client (ensure-client! service key request)]
    #_:clj-kondo/ignore
    (locking (:lock client)
      (let [client* (ensure-protocol-initialized! service client request)
            response (send-request! client*
                                    "thread/read"
                                    {:threadId codex-thread-id
                                     :includeTurns true})]
        (codec/normalize-thread-read-response response)))))

(defn set-thread-name!
  [service request]
  (let [codex-thread-id (or (desired-thread-id request)
                            (throw (ex-info "codex-thread-id required for thread/name/set"
                                            {:blame :caller})))
        thread-name (:thread-name request)
        key (require-session-key request)
        client (ensure-client! service key request)]
    (when-not (string? thread-name)
      (throw (ex-info "thread-name must be a string for thread/name/set"
                      {:blame :caller})))
    #_:clj-kondo/ignore
    (locking (:lock client)
      (let [client* (ensure-protocol-initialized! service client request)]
        (send-request! client*
                       "thread/name/set"
                       (thread-name-set-params codex-thread-id thread-name))
        {:thread-id codex-thread-id
         :thread-name thread-name}))))

(defn steer!
  "Send input items into the currently active turn for a session.
   Returns true if sent, nil if no active turn or on error."
  [service request]
  (try
    (let [key (require-session-key request)]
      (when-let [client (get @(:clients* service) key)]
        (when-let [turn-id (:active-turn-id client)]
          (let [id (next-id! (:id-atom client))]
            (send-json! (:writer client)
                        {:id id
                         :method "turn/steer"
                         :params {:threadId (:thread-id client)
                                  :input (require-input-items request)
                                  :expectedTurnId turn-id}})
            (log/debug (str "[CodexAppServer] steer sent, turn-id=" turn-id))
            true))))
    (catch Exception e
      (log/warn (str "[CodexAppServer] steer failed: " (.getMessage e)))
      nil)))
