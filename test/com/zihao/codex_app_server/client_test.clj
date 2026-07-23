(ns com.zihao.codex-app-server.client-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [com.zihao.codex-app-server.client :as sut]))

(deftest build-thread-start-params-fixed-policy-test
  (testing "approval/sandbox policy is fixed for non-interactive app-server use"
    (let [params (sut/build-thread-start-params
                  {}
                  {:workdir-path "/tmp/w"})]
      (is (= "never" (:approvalPolicy params)))
      (is (= "danger-full-access" (:sandbox params)))
      (is (not (contains? params :sandboxPolicy)))
      (is (= "/tmp/w" (:cwd params)))
      (is (= "gpt-5.5" (:model params))))))

(deftest build-thread-start-params-code-mode-raw-events-test
  (testing "Code Mode automatically subscribes new threads to raw response events"
    (is (= true
           (:experimentalRawEvents
            (sut/build-thread-start-params
             {:code-mode-only? true}
             {:workdir-path "/tmp/w"})))))
  (testing "normal app-server threads do not subscribe to raw response events"
    (is (not (contains? (sut/build-thread-start-params {} {})
                        :experimentalRawEvents)))))

(deftest dynamic-tools-explicit-false-throws-test
  (testing "dynamic-tools with explicit :experimental-api? false fails fast"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"dynamic-tools requires :experimental-api\? true"
         (sut/desired-experimental-api?
          {:experimental-api? false
           :dynamic-tools [{:name "lookup_ticket"
                            :description "Fetch ticket"
                            :input-schema {:type "object"}}]})))))

(deftest code-mode-explicit-experimental-api-false-throws-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"code-mode-only requires :experimental-api\? true"
       (sut/desired-experimental-api?
        {:experimental-api? false
         :code-mode-only? true}))))

(deftest ensure-thread-resumes-persisted-remote-thread-id
  (testing "a request with codex-thread-id resumes the remote thread instead of starting a fresh one"
    (let [service (sut/start! {})
          client {:session-key "feishu:chat-1"
                  :initialized? false
                  :thread-id nil
                  :id-atom (atom 0)
                  :lock (Object.)
                  :writer nil}
          calls (atom [])]
      (reset! (:clients* service) {"feishu:chat-1" client})
      (with-redefs [sut/send-request! (fn [_ method params]
                                        (swap! calls conj [method params])
                                        (case method
                                          "initialize" {:userAgent "ua"}
                                          "account/read" {:account {:id "a1"}}
                                          "thread/start" (throw (ex-info "must not start" {}))
                                          "thread/resume" {:thread {:id (:threadId params)
                                                                    :turns [{:id "turn-1"}
                                                                            {:id "turn-2"}]}}
                                          {}))
                    sut/send-json! (fn [& _] nil)]
        (let [result (#'sut/ensure-thread!
                      service
                      client
                      {:session-key "feishu:chat-1"
                       :codex-thread-id "remote-thread-1"})]
          (is (= "remote-thread-1" (:thread-id result)))
          (is (= 2 (:turn-count result)))
          (is (= ["initialize" "account/read" "thread/resume"]
                 (mapv first @calls)))
          (is (= {:threadId "remote-thread-1"
                  :approvalPolicy "never"
                  :sandbox "danger-full-access"
                  :cwd (.getAbsolutePath (io/file "."))
                  :model "gpt-5.5"}
                 (second (nth @calls 2)))))))))

(deftest run-turn-handles-dynamic-tool-call-request
  (testing "item/tool/call request triggers callback and JSON-RPC response"
    (let [service (sut/start! {})
          msgs (atom [{:id 60
                       :method "item/tool/call"
                       :params {:threadId "thr-1"
                                :turnId "turn-1"
                                :callId "call-1"
                                :tool "lookup_ticket"
                                :arguments {:id "ABC-123"}}}
                      {:method "turn/completed"
                       :params {:turn {:id "turn-1"
                                       :status "completed"}}}])
          sent (atom [])
          seen-call (atom nil)
          client {:session-key "feishu:chat-1"
                  :thread-id "thr-1"
                  :writer nil
                  :stdout-stream nil
                  :id-atom (atom 0)}]
      (reset! (:clients* service) {"feishu:chat-1" client})
      (with-redefs [sut/send-request! (fn [_ method _params]
                                        (if (= method "turn/start")
                                          {:turn {:id "turn-1"}}
                                          {}))
                    sut/read-next-msg! (fn [_]
                                         (let [m (first @msgs)]
                                           (swap! msgs #(vec (rest %)))
                                           m))
                    sut/send-json! (fn [_ payload]
                                     (swap! sent conj payload))]
        (let [result (#'sut/run-turn-on-client!
                      service
                      client
                      {:session-key "feishu:chat-1"
                       :input-items [{:type "text"
                                      :text "hi"
                                      :text_elements []}]
                       :callbacks {:on-dynamic-tool-call
                                   (fn [call]
                                     (reset! seen-call call)
                                     {:success true
                                      :content-items [{:type "inputText"
                                                       :text "ticket open"}]})}})]
          (is (= :completed (:status result)))
          (is (= {:thread-id "thr-1"
                  :turn-id "turn-1"
                  :call-id "call-1"
                  :tool "lookup_ticket"
                  :arguments {:id "ABC-123"}}
                 @seen-call))
          (is (= [{:id 60
                   :result {:success true
                            :contentItems [{:type "inputText"
                                            :text "ticket open"}]}}]
                 @sent)))))))

(deftest run-turn-emits-command-item-callbacks
  (testing "app-server command execution wire events reach item and command callbacks"
    (let [service (sut/start! {})
          msgs (atom [{:method "item/started"
                       :params {:item {:type "commandExecution"
                                       :id "cmd-1"
                                       :commandActions [{:command "ls"}]}}}
                      {:method "item/commandExecution/outputDelta"
                       :params {:delta "apps\n"}}
                      {:method "item/completed"
                       :params {:item {:type "commandExecution"
                                       :id "cmd-1"
                                       :status "completed"
                                       :commandActions [{:command "ls"}]
                                       :exitCode 0
                                       :aggregatedOutput "apps\nbases\n"}}}
                      {:method "turn/completed"
                       :params {:turn {:id "turn-1"
                                       :status "completed"}}}])
          seen (atom [])
          client {:session-key "feishu:chat-1"
                  :thread-id "thr-1"
                  :writer nil
                  :stdout-stream nil
                  :id-atom (atom 0)}]
      (reset! (:clients* service) {"feishu:chat-1" client})
      (with-redefs [sut/send-request! (fn [_ method _params]
                                        (if (= method "turn/start")
                                          {:turn {:id "turn-1"}}
                                          {}))
                    sut/read-next-msg! (fn [_]
                                         (let [m (first @msgs)]
                                           (swap! msgs #(vec (rest %)))
                                           m))]
        (let [result (#'sut/run-turn-on-client!
                      service
                      client
                      {:session-key "feishu:chat-1"
                       :input-items [{:type "text"
                                      :text "try use ls"
                                      :text_elements []}]
                       :callbacks {:on-item-started #(swap! seen conj [:started %])
                                   :on-command-delta #(swap! seen conj [:delta %])
                                   :on-item-completed #(swap! seen conj [:completed %])}})]
          (is (= :completed (:status result)))
          (is (= [[:started {:item-type "commandExecution"
                             :item-id "cmd-1"
                             :command "ls"}]
                  [:delta "apps\n"]
                  [:completed {:item-type "commandExecution"
                               :item-id "cmd-1"
                               :status "completed"
                               :command "ls"
                               :exit-code 0
                               :output "apps\nbases\n"}]]
                 @seen)))))))

(deftest run-turn-emits-correlated-code-mode-item-callbacks
  (testing "raw exec source and output become one stable Code Mode item"
    (let [service (sut/start! {})
          msgs (atom [{:method "rawResponseItem/completed"
                       :params {:threadId "thr-1"
                                :turnId "turn-1"
                                :item {:type "custom_tool_call"
                                       :id "ctc-1"
                                       :call_id "call-1"
                                       :name "exec"
                                       :input "text(ALL_TOOLS.length);"}}}
                      {:method "rawResponseItem/completed"
                       :params {:threadId "thr-1"
                                :turnId "turn-1"
                                :item {:type "custom_tool_call_output"
                                       :call_id "call-1"
                                       :output [{:type "input_text"
                                                 :text "Script completed\n7"}]}}}
                      {:method "turn/completed"
                       :params {:turn {:id "turn-1"
                                       :status "completed"}}}])
          seen (atom [])
          client {:session-key "web:thread-1"
                  :thread-id "thr-1"
                  :writer nil
                  :stdout-stream nil
                  :id-atom (atom 0)}]
      (reset! (:clients* service) {"web:thread-1" client})
      (with-redefs [sut/send-request! (fn [_ method _params]
                                        (if (= method "turn/start")
                                          {:turn {:id "turn-1"}}
                                          {}))
                    sut/read-next-msg! (fn [_]
                                         (let [m (first @msgs)]
                                           (swap! msgs #(vec (rest %)))
                                           m))]
        (#'sut/run-turn-on-client!
         service
         client
         {:session-key "web:thread-1"
          :input-items [{:type "text" :text "prove it" :text_elements []}]
          :callbacks {:on-item-started #(swap! seen conj [:started %])
                      :on-item-completed #(swap! seen conj [:completed %])}})
        (is (= [[:started {:item-type "codeMode"
                            :item-id "ctc-1"
                            :status "running"
                            :codex-turn-id "turn-1"
                            :call-id "call-1"
                            :language "javascript"
                            :source "text(ALL_TOOLS.length);"}]
                [:completed {:item-type "codeMode"
                              :item-id "ctc-1"
                              :status "completed"
                              :codex-turn-id "turn-1"
                              :call-id "call-1"
                              :language "javascript"
                              :source "text(ALL_TOOLS.length);"
                              :output "Script completed\n7"}]]
               @seen))))))

(deftest interrupt-turn-sends-active-turn-interrupt
  (testing "interrupt-turn writes the active turn interrupt without waiting for a response"
    (let [service (sut/start! {})
          sent (atom [])
          client {:session-key "feishu:chat-1"
                  :thread-id "thr-1"
                  :active-turn-id "turn-1"
                  :writer nil
                  :id-atom (atom 0)}]
      (reset! (:clients* service) {"feishu:chat-1" client})
      (with-redefs [sut/send-json! (fn [_writer payload]
                                     (swap! sent conj payload))]
        (is (= {:interrupted? true
                :codex-thread-id "thr-1"
                :codex-turn-id "turn-1"}
               (sut/interrupt-turn! service {:session-key "feishu:chat-1"})))
        (is (= [{:id 1
                 :method "turn/interrupt"
                 :params {:threadId "thr-1"
                          :turnId "turn-1"}}]
               @sent)))))
  (testing "sessions without an active turn are reported without writing"
    (let [service (sut/start! {})
          sent (atom [])]
      (reset! (:clients* service) {"feishu:chat-1" {:session-key "feishu:chat-1"
                                                    :thread-id "thr-1"
                                                    :id-atom (atom 0)}})
      (with-redefs [sut/send-json! (fn [_writer payload]
                                     (swap! sent conj payload))]
        (is (= {:interrupted? false
                :reason :no-active-turn
                :codex-thread-id "thr-1"}
               (sut/interrupt-turn! service {:session-key "feishu:chat-1"})))
        (is (= [] @sent))))))
