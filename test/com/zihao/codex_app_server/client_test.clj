(ns com.zihao.codex-app-server.client-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [com.zihao.codex-app-server.client :as sut]))

(deftest build-thread-start-params-fixed-policy-test
  (testing "approval/sandbox policy is fixed for non-interactive app-server use"
    (let [params (sut/build-thread-start-params
                  {:model "gpt-5-codex"}
                  {:workdir-path "/tmp/w"})]
      (is (= "never" (:approvalPolicy params)))
      (is (= "danger-full-access" (:sandbox params)))
      (is (not (contains? params :sandboxPolicy)))
      (is (= "/tmp/w" (:cwd params)))
      (is (= "gpt-5-codex" (:model params))))))

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
                  :model "gpt-5-codex"}
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
