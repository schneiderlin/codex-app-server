(ns com.zihao.codex-app-server.process-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [com.zihao.codex-app-server.process :as sut]))

(deftest codex-app-server-command-cross-platform-test
  (testing "unix-like systems launch codex directly"
    (is (= ["codex" "app-server" "--listen" "stdio://"]
           (sut/codex-app-server-command "Linux" nil)))
    (is (= ["codex" "app-server" "--listen" "stdio://"]
           (sut/codex-app-server-command "Mac OS X" "C:\\Windows\\System32\\cmd.exe"))))
  (testing "windows goes through cmd so PATH and PATHEXT can resolve codex.cmd"
    (is (= ["cmd.exe" "/d" "/c" "codex" "app-server" "--listen" "stdio://"]
           (sut/codex-app-server-command "Windows 11" nil)))
    (is (= ["C:\\Windows\\System32\\cmd.exe" "/d" "/c" "codex" "app-server" "--listen" "stdio://"]
           (sut/codex-app-server-command "Windows 11" "C:\\Windows\\System32\\cmd.exe")))))

(deftest codex-app-server-command-code-mode-only-test
  (testing "the feature is omitted by default and when explicitly disabled"
    (doseq [config [{} {:code-mode-only? false}]]
      (is (= ["codex" "app-server" "--listen" "stdio://"]
             (sut/codex-app-server-command config "Linux" nil)))))
  (testing "the feature can be enabled on unix-like systems"
    (is (= ["codex" "app-server" "--enable" "code_mode_only" "--listen" "stdio://"]
           (sut/codex-app-server-command {:code-mode-only? true} "Linux" nil))))
  (testing "the feature can be enabled through the Windows command wrapper"
    (is (= ["cmd.exe" "/d" "/c" "codex" "app-server" "--enable" "code_mode_only" "--listen" "stdio://"]
           (sut/codex-app-server-command {:code-mode-only? true} "Windows 11" nil)))))

(deftest resolve-codex-home-test
  (testing "explicit config wins"
    (is (= "/tmp/custom-codex-home"
           (sut/resolve-codex-home {:codex-home "/tmp/custom-codex-home"}
                                   "/tmp/env-codex-home"
                                   "/home/u"))))
  (testing "environment CODEX_HOME is used when config omits it"
    (is (= "/tmp/env-codex-home"
           (sut/resolve-codex-home {}
                                   "/tmp/env-codex-home"
                                   "/home/u"))))
  (testing "fallback matches the Codex CLI user home"
    (is (= (.getAbsolutePath (io/file "/home/u" ".codex"))
           (sut/resolve-codex-home {}
                                   nil
                                   "/home/u")))))

(deftest configure-codex-app-server-environment-test
  (testing "Codex app-server uses Codex home auth instead of inherited OpenAI API env"
    (let [env (java.util.HashMap. {"OPENAI_API_KEY" "sk-test"
                                   "OPENAI_BASE_URL" "https://api-proxy.invalid/v1"
                                   "OPENAI_API_BASE" "https://api-base.invalid/v1"
                                   "OPENAI_ORG_ID" "org-test"
                                   "OPENAI_ORGANIZATION" "org-test"
                                   "OPENAI_PROJECT" "proj-test"
                                   "CODEX_SANDBOX_NETWORK_DISABLED" "1"
                                   "PATH" "/usr/bin"})]
      (sut/configure-codex-app-server-environment! env "/home/u/.codex")
      (is (= "/home/u/.codex" (.get env "CODEX_HOME")))
      (is (= "/usr/bin" (.get env "PATH")))
      (doseq [env-var ["OPENAI_API_KEY"
                       "OPENAI_BASE_URL"
                       "OPENAI_API_BASE"
                       "OPENAI_ORG_ID"
                       "OPENAI_ORGANIZATION"
                       "OPENAI_PROJECT"
                       "CODEX_SANDBOX_NETWORK_DISABLED"]]
        (is (false? (.containsKey env env-var)))))))
