(ns com.zihao.codex-app-server.interface
  (:require [com.zihao.codex-app-server.client :as client]
            [com.zihao.codex-app-server.codec :as codec]))

(defn start!
  [config]
  (client/start! config))

(defn close!
  [service]
  (client/close! service))

(defn run-turn!
  [service request]
  (client/run-turn! service request))

(defn read-thread
  [service request]
  (client/read-thread service request))

(defn set-thread-name!
  [service request]
  (client/set-thread-name! service request))

(defn steer!
  [service request]
  (client/steer! service request))

(defn interrupt-turn!
  [service request]
  (client/interrupt-turn! service request))

(def parse-json-safe codec/parse-json-safe)
(def message-role codec/message-role)
(def user-message->input-items codec/user-message->input-items)
(def latest-user-input-items codec/latest-user-input-items)
(def current-turn-user-input-items codec/current-turn-user-input-items)
(def agent-message-delta codec/agent-message-delta)
(def agent-delta-text codec/agent-delta-text)
(def reasoning-delta-text codec/reasoning-delta-text)
(def completed-agent-message codec/completed-agent-message)
(def completed-agent-message-text codec/completed-agent-message-text)
(def turn-completed? codec/turn-completed?)
(def turn-completed-status codec/turn-completed-status)
(def turn-completed-error-message codec/turn-completed-error-message)
(def terminal-error-for-turn codec/terminal-error-for-turn)
(def usage-from-thread-token-updated codec/usage-from-thread-token-updated)
(def thread-display-name codec/thread-display-name)
(def thread-name-updated codec/thread-name-updated)
(def item-started codec/item-started)
(def item-completed codec/item-completed)
(def command-output-delta codec/command-output-delta)
(def file-change-output-delta codec/file-change-output-delta)
(def plan-delta codec/plan-delta)
(def item-started-command codec/item-started-command)
(def item-completed-command-info codec/item-completed-command-info)
(def item-file-change-info codec/item-file-change-info)
(def item-completed-file-change-info codec/item-completed-file-change-info)
(def item-started-dynamic-tool-info codec/item-started-dynamic-tool-info)
(def item-completed-dynamic-tool-info codec/item-completed-dynamic-tool-info)
(def item-image-generation-info codec/item-image-generation-info)
(def dynamic-tool-call-request codec/dynamic-tool-call-request)
(def normalize-dynamic-tool-content-item codec/normalize-dynamic-tool-content-item)
(def normalize-dynamic-tool-content-items codec/normalize-dynamic-tool-content-items)
(def dynamic-tool-call-response codec/dynamic-tool-call-response)
(def normalize-thread-read-response codec/normalize-thread-read-response)
(def normalized-thread-read-running? codec/normalized-thread-read-running?)
