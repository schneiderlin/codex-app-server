(ns com.zihao.codex-app-server.codec
  "Pure helpers for Codex app-server protocol payloads."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [com.zihao.agent-common.interface :as cb]))

(declare normalize-dynamic-tool-content-item)

(defn- type->string
  [t]
  (cond
    (keyword? t) (name t)
    (string? t) t
    (nil? t) ""
    :else (str t)))

(defn parse-json-safe
  [s]
  (try
    (json/read-str s :key-fn keyword)
    (catch Exception _
      nil)))

(defn message-role
  [m]
  (let [r (:role m)]
    (cond
      (keyword? r) (name r)
      (string? r) r
      :else nil)))

(defn- normalize-content
  [content]
  (cb/normalize-content content))

(defn- image-source->url
  [source]
  (let [source-type (type->string (:type source))]
    (case source-type
      "base64"
      (let [media-type (:media_type source)
            data (:data source)]
        (when (and (not (str/blank? (str media-type)))
                   (not (str/blank? (str data))))
          (str "data:" media-type ";base64," data)))

      "data_url"
      (or (:data_url source) (:url source))

      "url"
      (:url source)

      "asset"
      (let [media-type (:media_type source)
            data (:data source)]
        (when (and (not (str/blank? (str media-type)))
                   (not (str/blank? (str data))))
          (str "data:" media-type ";base64," data)))

      nil)))

(defn- block->turn-input-item
  [block]
  (let [t (type->string (:type block))]
    (case t
      "text"
      {:type "text"
       :text (or (:text block) "")
       :text_elements []}

      "image"
      (when-let [url (image-source->url (:source block))]
        {:type "image"
         :url url})

      nil)))

(defn user-message->input-items
  [msg]
  (let [blocks (normalize-content (:content msg))
        input-items (->> blocks
                         (keep block->turn-input-item)
                         vec)]
    (if (seq input-items)
      input-items
      [{:type "text"
        :text (let [text (cb/extract-text blocks)]
                (if (str/blank? text)
                  (pr-str blocks)
                  text))
        :text_elements []}])))

(defn latest-user-input-items
  [messages]
  (let [user-msg (last (filter #(= "user" (message-role %)) messages))]
    (when user-msg
      (user-message->input-items user-msg))))

(defn current-turn-user-input-items
  "Return input items for all user messages after the last assistant message."
  [messages]
  (let [tail-messages (->> (or messages [])
                           reverse
                           (take-while #(not= "assistant" (message-role %)))
                           reverse)
        input-items (vec (mapcat user-message->input-items
                                 (filter #(= "user" (message-role %)) tail-messages)))]
    (when (seq input-items)
      input-items)))

(defn agent-message-delta
  [msg]
  (when (= "item/agentMessage/delta" (:method msg))
    (let [params (:params msg)]
      {:thread-id (:threadId params)
       :turn-id (:turnId params)
       :item-id (:itemId params)
       :delta (:delta params)})))

(defn agent-delta-text
  [msg]
  (:delta (agent-message-delta msg)))

(defn reasoning-delta-text
  [msg]
  (when (= "item/reasoning/summaryTextDelta" (:method msg))
    (get-in msg [:params :delta])))

(defn completed-agent-message
  [msg]
  (let [method (:method msg)
        item (get-in msg [:params :item])]
    (when (and (= "item/completed" method)
               (= "agentMessage" (:type item)))
      {:item-id (:id item)
       :text (:text item)
       :status (:status item)})))

(defn completed-agent-message-text
  [msg]
  (:text (completed-agent-message msg)))

(defn turn-completed?
  [msg turn-id]
  (and (= "turn/completed" (:method msg))
       (= turn-id (get-in msg [:params :turn :id]))))

(defn turn-completed-status
  [msg]
  (get-in msg [:params :turn :status]))

(defn turn-completed-error-message
  [msg]
  (or (get-in msg [:params :turn :error :message])
      (get-in msg [:params :turn :error :error])))

(defn terminal-error-for-turn
  [msg turn-id]
  (when (and (= "error" (:method msg))
             (= turn-id (get-in msg [:params :turnId]))
             (false? (get-in msg [:params :willRetry])))
    (or (get-in msg [:params :error :message])
        (get-in msg [:params :error :additionalDetails]))))

(defn usage-from-thread-token-updated
  [msg]
  (when (= "thread/tokenUsage/updated" (:method msg))
    (let [last-usage (get-in msg [:params :tokenUsage :last])]
      (when (map? last-usage)
        {:input_tokens (or (:inputTokens last-usage) 0)
         :output_tokens (or (:outputTokens last-usage) 0)
         :cache_read_tokens (or (:cachedInputTokens last-usage) 0)
         :cache_creation_tokens 0}))))

(defn thread-display-name
  "Extract the app-server thread name from current and legacy response shapes."
  [thread]
  (or (:name thread)
      (:title thread)))

(defn thread-name-updated
  "Parse `thread/name/updated` notification."
  [msg]
  (when (= "thread/name/updated" (:method msg))
    (let [params (:params msg)]
      {:thread-id (:threadId params)
       :thread-name (or (:threadName params)
                        (:thread-name params)
                        (:name params)
                        (:title params))})))

(defn item-started
  [msg]
  (when (= "item/started" (:method msg))
    (let [item (get-in msg [:params :item])]
      (when (map? item)
        {:item-type (:type item)
         :item-id (:id item)
         :data item}))))

(defn item-completed
  [msg]
  (when (= "item/completed" (:method msg))
    (let [item (get-in msg [:params :item])]
      (when (map? item)
        {:item-type (:type item)
         :item-id (:id item)
         :status (:status item)
         :data item}))))

(defn code-mode-call
  "Parse the internal raw response notification for a Code Mode exec call."
  [msg]
  (let [params (:params msg)
        item (:item params)
        call-id (:call_id item)
        turn-id (:turnId params)
        source (:input item)]
    (when (and (= "rawResponseItem/completed" (:method msg))
               (= "custom_tool_call" (:type item))
               (= "exec" (:name item))
               (not (str/blank? (str call-id)))
               (not (str/blank? (str turn-id)))
               (string? source))
      {:item-type "codeMode"
       :item-id (or (:id item) (str "code-mode:" turn-id ":" call-id))
       :status "running"
       :codex-turn-id turn-id
       :call-id call-id
       :language "javascript"
       :source source})))

(defn- code-mode-output-text
  [output]
  (cond
    (string? output)
    output

    (sequential? output)
    (->> output
         (keep (fn [content-item]
                 (when (and (map? content-item)
                            (string? (:text content-item)))
                   (:text content-item))))
         (str/join ""))

    :else nil))

(defn code-mode-output
  "Parse the internal raw response notification for a Code Mode exec output."
  [msg]
  (let [params (:params msg)
        item (:item params)
        call-id (:call_id item)
        turn-id (:turnId params)
        output (code-mode-output-text (:output item))]
    (when (and (= "rawResponseItem/completed" (:method msg))
               (= "custom_tool_call_output" (:type item))
               (not (str/blank? (str call-id)))
               (not (str/blank? (str turn-id)))
               (some? output))
      {:codex-turn-id turn-id
       :call-id call-id
       :output output})))

(defn command-output-delta
  [msg]
  (when (= "item/commandExecution/outputDelta" (:method msg))
    (get-in msg [:params :delta])))

(defn file-change-output-delta
  [msg]
  (when (= "item/fileChange/outputDelta" (:method msg))
    (get-in msg [:params :delta])))

(defn plan-delta
  [msg]
  (when (= "item/plan/delta" (:method msg))
    (get-in msg [:params :delta])))

(defn item-started-command
  [item-data]
  (or (some-> (:commandActions item-data) first :command)
      (:command item-data)))

(defn item-completed-command-info
  [item-data]
  (when (= "commandExecution" (:type item-data))
    {:command (item-started-command item-data)
     :exit-code (:exitCode item-data)
     :output (or (:aggregatedOutput item-data) (:output item-data))}))

(defn- file-change-path
  [item]
  (or (:filePath item)
      (:file item)
      (:path item)
      (some (fn [change]
              (or (:path change)
                  (:filePath change)
                  (:file change)))
            (:changes item))))

(defn- file-change-patch
  [item]
  (or (:patch item)
      (when (contains? item :changes)
        (pr-str (:changes item)))))

(defn item-file-change-info
  [item-data]
  (when (= "fileChange" (:type item-data))
    (cond-> {}
      (some? (file-change-path item-data)) (assoc :file-path (file-change-path item-data))
      (some? (file-change-patch item-data)) (assoc :patch (file-change-patch item-data)))))

(defn item-completed-file-change-info
  [item-data]
  (item-file-change-info item-data))

(defn item-started-dynamic-tool-info
  [item-data]
  (when (= "dynamicToolCall" (:type item-data))
    {:tool (:tool item-data)
     :arguments (:arguments item-data)}))

(defn item-completed-dynamic-tool-info
  [item-data]
  (when (= "dynamicToolCall" (:type item-data))
    {:tool (:tool item-data)
     :arguments (:arguments item-data)
     :success (:success item-data)
     :content-items (->> (or (:contentItems item-data) [])
                         (map normalize-dynamic-tool-content-item)
                         (remove nil?)
                         vec)}))

(defn item-image-generation-info
  [item-data]
  (when (= "imageGeneration" (:type item-data))
    (cond-> {}
      (contains? item-data :status)
      (assoc :status (:status item-data))

      (contains? item-data :revisedPrompt)
      (assoc :revised-prompt (:revisedPrompt item-data))

      (contains? item-data :result)
      (assoc :result (:result item-data))

      (contains? item-data :savedPath)
      (assoc :saved-path (:savedPath item-data)))))

(defn dynamic-tool-call-request
  [msg]
  (let [params (:params msg)]
    (when (and (= "item/tool/call" (:method msg))
               (some? (:id msg))
               (map? params))
      {:request-id (:id msg)
       :thread-id (:threadId params)
       :turn-id (:turnId params)
       :call-id (:callId params)
       :tool (:tool params)
       :arguments (:arguments params)})))

(defn normalize-dynamic-tool-content-item
  [item]
  (when (map? item)
    (let [item-type (:type item)]
      (cond
        (and (= "inputText" item-type)
             (string? (:text item)))
        {:type "inputText"
         :text (:text item)}

        (and (= "inputImage" item-type)
             (string? (:imageUrl item)))
        {:type "inputImage"
         :imageUrl (:imageUrl item)}

        :else nil))))

(defn normalize-dynamic-tool-content-items
  [items]
  (->> (or items [])
       (map normalize-dynamic-tool-content-item)
       (remove nil?)
       vec))

(defn dynamic-tool-call-response
  [request-id result-map]
  (let [success (boolean (:success result-map))
        content-items (normalize-dynamic-tool-content-items (:content-items result-map))]
    {:id request-id
     :result {:success success
              :contentItems content-items}}))

(defn- status->transcript-status
  [status]
  (let [s (type->string status)]
    (case s
      "running" :running
      "inProgress" :running
      "failed" :failed
      "declined" :failed
      "interrupted" :failed
      "completed" :completed
      :completed)))

(defn- status-type
  [status]
  (cond
    (map? status) (or (:type status) (get-in status [:root :type]))
    (keyword? status) (name status)
    (string? status) status
    :else nil))

(defn- remote-item-base
  [turn-id item]
  {:codex-turn-id turn-id
   :codex-item-id (:id item)
   :codex-item-type (:type item)
   :codex-item-status (status->transcript-status (:status item))})

(defn- remote-command-entry
  [turn-id item]
  (merge {:entry-type :codex-item
          :item-type "commandExecution"
          :item-id (:id item)
          :status (status->transcript-status (:status item))}
         (remote-item-base turn-id item)
         (cond-> {}
           (some? (item-started-command item)) (assoc :command (item-started-command item))
           (or (contains? item :aggregatedOutput)
               (contains? item :output)) (assoc :output (or (:aggregatedOutput item) (:output item)))
           (contains? item :exitCode) (assoc :exit-code (:exitCode item)))))

(defn- remote-file-change-entry
  [turn-id item]
  (merge {:entry-type :codex-item
          :item-type "fileChange"
          :item-id (:id item)
          :status (status->transcript-status (:status item))}
         (remote-item-base turn-id item)
         (cond-> {}
           (some? (file-change-path item)) (assoc :file-path (file-change-path item))
           (some? (file-change-patch item)) (assoc :patch (file-change-patch item)))))

(defn- remote-dynamic-tool-entry
  [turn-id item]
  (merge {:entry-type :codex-item
          :item-type "dynamicToolCall"
          :item-id (:id item)
          :status (status->transcript-status (:status item))}
         (remote-item-base turn-id item)
         (cond-> {}
           (contains? item :tool) (assoc :tool (:tool item))
           (contains? item :arguments) (assoc :arguments (:arguments item))
           (contains? item :success) (assoc :success (:success item))
           (contains? item :contentItems)
           (assoc :content-items (normalize-dynamic-tool-content-items (:contentItems item))))))

(defn- remote-image-generation-entry
  [turn-id item]
  (merge {:entry-type :codex-item
          :item-type "imageGeneration"
          :item-id (:id item)
          :status (status->transcript-status (:status item))}
         (remote-item-base turn-id item)
         (cond-> {}
           (contains? item :revisedPrompt) (assoc :revised-prompt (:revisedPrompt item))
           (contains? item :result) (assoc :result (:result item))
           (contains? item :savedPath) (assoc :saved-path (:savedPath item)))))

(defn- remote-agent-message-entry
  [turn-id item]
  (when-not (str/blank? (str (or (:text item) "")))
    (merge {:role "assistant"
            :content [{:type "text" :text (:text item)}]}
           (remote-item-base turn-id item)
           (when (some? (:phase item))
             {:phase (:phase item)}))))

(defn- remote-item-observation
  [turn-id item observation-type]
  (cond-> {:type observation-type
           :turn-id turn-id
           :item-id (:id item)
           :item-type (:type item)}
    (:status item) (assoc :status (:status item))))

(defn- normalize-remote-item
  [turn-id item]
  (let [item-type (:type item)]
    (case item-type
      "agentMessage"
      {:entry (remote-agent-message-entry turn-id item)}

      "commandExecution"
      {:entry (remote-command-entry turn-id item)}

      "fileChange"
      {:entry (remote-file-change-entry turn-id item)}

      "dynamicToolCall"
      {:entry (remote-dynamic-tool-entry turn-id item)}

      "imageGeneration"
      {:entry (remote-image-generation-entry turn-id item)}

      "contextCompaction"
      {:observation (remote-item-observation turn-id item :context-compaction)}

      "userMessage"
      nil

      {:observation (remote-item-observation turn-id item :unsupported-remote-item)})))

(defn- thread-read-running?
  [thread-status turns]
  (or (= "active" (status-type thread-status))
      (some (fn [turn]
              (= "inProgress" (type->string (:status turn))))
            turns)))

(defn normalize-thread-read-response
  "Normalize `thread/read includeTurns=true` into reconcile-ready entries."
  [response]
  (let [thread (:thread response)
        turns (vec (or (:turns thread) []))
        normalized-turns (mapv (fn [turn]
                                 {:turn-id (:id turn)
                                  :status (:status turn)
                                  :items (vec (or (:items turn) []))})
                               turns)
        item-results (mapcat (fn [turn]
                               (keep #(normalize-remote-item (:id turn) %)
                                     (or (:items turn) [])))
                             turns)]
    {:thread-id (:id thread)
     :thread-title (thread-display-name thread)
     :thread-status (:status thread)
     :turns normalized-turns
     :remote-running? (boolean (thread-read-running? (:status thread) normalized-turns))
     :entries (vec (keep :entry item-results))
     :observations (vec (keep :observation item-results))}))

(defn normalized-thread-read-running?
  [normalized-read]
  (boolean (thread-read-running? (:thread-status normalized-read)
                                 (:turns normalized-read))))
