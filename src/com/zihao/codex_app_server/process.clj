(ns com.zihao.codex-app-server.process
  "Codex app-server process lifecycle helpers."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.zihao.agent-common.interface :as log]))

(def ^:private openai-api-env-vars
  ["OPENAI_API_KEY"
   "OPENAI_BASE_URL"
   "OPENAI_API_BASE"
   "OPENAI_ORG_ID"
   "OPENAI_ORGANIZATION"
   "OPENAI_PROJECT"])

(defn windows-os-name?
  [os-name]
  (str/includes? (str/lower-case (str os-name)) "win"))

(defn codex-app-server-command
  ([]
   (codex-app-server-command (System/getProperty "os.name")
                             (System/getenv "ComSpec")))
  ([os-name windows-comspec]
   (let [args ["codex" "app-server" "--listen" "stdio://"]]
     (if (windows-os-name? os-name)
       (into [(or (some-> windows-comspec str/trim not-empty)
                  "cmd.exe")
              "/d"
              "/c"]
             args)
       args))))

(defn resolve-codex-home
  ([config]
   (resolve-codex-home config
                       (System/getenv "CODEX_HOME")
                       (System/getProperty "user.home")))
  ([config env-codex-home user-home]
   (.getAbsolutePath
    (io/file
     (or (some-> (:codex-home config) str/trim not-empty)
         (some-> env-codex-home str/trim not-empty)
         (str (io/file user-home ".codex")))))))

(defn configure-codex-app-server-environment!
  [^java.util.Map env codex-home]
  (doseq [env-var openai-api-env-vars]
    (.remove env env-var))
  (when (.containsKey env "CODEX_SANDBOX_NETWORK_DISABLED")
    (.remove env "CODEX_SANDBOX_NETWORK_DISABLED"))
  (.put env "CODEX_HOME" codex-home)
  env)

(defn start-codex-app-server-process!
  ([config]
   (start-codex-app-server-process! config (resolve-codex-home config)))
  ([_config codex-home]
   (let [pb (ProcessBuilder. (codex-app-server-command))
         env (.environment pb)]
     (.mkdirs (io/file codex-home))
     (configure-codex-app-server-environment! env codex-home)
     (.redirectErrorStream pb false)
     (.start pb))))

(defn start-stderr-pump!
  [^Process process]
  (future
    (try
      (with-open [r (io/reader (.getErrorStream process))]
        (loop []
          (when-let [line (.readLine ^java.io.BufferedReader r)]
            (log/debug (str "[CodexAppServer][stderr] " line))
            (recur))))
      (catch Exception _
        nil))))

(defn destroy-client!
  [client]
  (when client
    (try (.close ^java.io.Writer (:writer client)) (catch Exception _ nil))
    (try (.close ^java.io.BufferedReader (:reader client)) (catch Exception _ nil))
    (try (.destroy ^Process (:process client)) (catch Exception _ nil))))
