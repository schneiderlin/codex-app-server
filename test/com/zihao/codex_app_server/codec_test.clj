(ns com.zihao.codex-app-server.codec-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.zihao.codex-app-server.codec :as codec]))

(deftest dynamic-tool-call-request-parse-test
  (testing "item/tool/call request is parsed into a stable map"
    (is (= {:request-id 60
            :thread-id "thr_123"
            :turn-id "turn_123"
            :call-id "call_123"
            :tool "lookup_ticket"
            :arguments {:id "ABC-123"}}
           (codec/dynamic-tool-call-request
            {:id 60
             :method "item/tool/call"
             :params {:threadId "thr_123"
                      :turnId "turn_123"
                      :callId "call_123"
                      :tool "lookup_ticket"
                      :arguments {:id "ABC-123"}}})))))

(deftest dynamic-tool-call-response-test
  (testing "builds JSON-RPC response with normalized content items"
    (is (= {:id 99
            :result {:success true
                     :contentItems [{:type "inputText" :text "done"}]}}
           (codec/dynamic-tool-call-response
            99
            {:success true
             :content-items [{:type "inputText" :text "done"}
                             {:type "invalid"}]})))))

(deftest normalize-thread-read-response-keeps-recoverable-entries-only
  (testing "thread/read turns become append-only entries while compaction stays hidden"
    (let [normalized (codec/normalize-thread-read-response
                      {:thread {:id "remote-thread-1"
                                :name "Remote title"
                                :status {:type "active"}
                                :turns [{:id "turn-1"
                                         :status "completed"
                                         :items [{:type "userMessage"
                                                  :id "user-1"
                                                  :text "run pwd"}
                                                 {:type "commandExecution"
                                                  :id "cmd-1"
                                                  :status "completed"
                                                  :commandActions [{:command "pwd"}]
                                                  :exitCode 0
                                                  :aggregatedOutput "/repo\n"}
                                                 {:type "agentMessage"
                                                  :id "msg-1"
                                                  :text "done"}
                                                 {:type "contextCompaction"
                                                  :id "compact-1"
                                                  :status "completed"}]}
                                        {:id "turn-2"
                                         :status "inProgress"
                                         :items []}]}})]
      (is (= "remote-thread-1" (:thread-id normalized)))
      (is (= "Remote title" (:thread-title normalized)))
      (is (= true (:remote-running? normalized)))
      (is (= ["cmd-1" "msg-1"] (mapv :codex-item-id (:entries normalized))))
      (is (= [{:type :context-compaction
               :turn-id "turn-1"
               :item-id "compact-1"
               :item-type "contextCompaction"
               :status "completed"}]
             (:observations normalized))))))

(deftest user-message->input-items-supports-text-and-image
  (testing "turn input items include both text and image URL"
    (is (= [{:type "text" :text "look" :text_elements []}
            {:type "image" :url "data:image/png;base64,AAA"}]
           (codec/user-message->input-items
            {:role "user"
             :content [{:type "text" :text "look"}
                       {:type "image"
                        :source {:type "base64"
                                 :media_type "image/png"
                                 :data "AAA"}}]})))))

(deftest current-turn-user-input-items-includes-runtime-context-after-real-user
  (testing "turn/start input includes all user messages since the last assistant"
    (is (= [{:type "text" :text "$demo" :text_elements []}
            {:type "text" :text "<skill>body</skill>" :text_elements []}]
           (codec/current-turn-user-input-items
            [{:role "user"
              :content [{:type "text" :text "previous"}]}
             {:role "assistant"
              :content [{:type "text" :text "ack"}]}
             {:role "user"
              :content [{:type "text" :text "$demo"}]}
             {:role "user"
              :source :runtime
              :message-type :skill-injection
              :content [{:type "text" :text "<skill>body</skill>"}]}])))))
