(ns clj-bugsnag.core
  (:require [clj-bugsnag.impl :as impl]
            [clj-stacktrace.core :refer [parse-exception]]
            [clj-stacktrace.repl :refer [method-str]]
            [clojure.java.shell :refer [sh]]
            [clj-http.client :as http]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(def UNHANDLED_EXCEPTION "unhandledException")
(def UNHANDLED_EXCEPTION_MIDDLEWARE "unhandledExceptionMiddleware")
(def HANDLED_EXCEPTION "handledException")

(defn get-git-rev
  []
  (try
    (string/trim (:out (sh "git" "rev-parse" "HEAD")))
    (catch Throwable _t "git revision not available")))

(def git-rev (memoize get-git-rev))

(defn get-hostname
  "Attempt to get the current hostname."
  []
  (try
    (.. java.net.InetAddress getLocalHost getHostName)
    (catch Throwable _t "Hostname could not be resolved")))

(defn- find-source-snippet
  [around function-name]
  (try
    (let [fn-sym (symbol function-name)
          fn-var (find-var fn-sym)
          source (repl/source-fn fn-sym)
          start (-> fn-var meta :line)
          indexed-lines (map-indexed (fn [i line]
                                       [(+ i start) (string/trimr line)])
                                     (string/split-lines source))]
      (into {} (filter #(<= (- around 3) (first %) (+ around 3)) indexed-lines)))
    (catch Exception _ex
      nil)))

(defn- transform-stacktrace
  [trace-elems project-ns]
  (try
    (vec (for [{:keys [file line ns] :as elem} trace-elems
               :let [project? (string/starts-with? (or ns "_") project-ns)
                     method (method-str elem)
                     code (when (string/ends-with? (or file "") ".clj")
                            (find-source-snippet line (string/replace (or method "") "[fn]" "")))]]
           {:file file
            :lineNumber line
            :method method
            :inProject project?
            :code code}))
    (catch Exception ex
      [{:file "clj-bugsnag/core.clj"
        :lineNumber 1
        :code {1 (str ex)
               2 "thrown while building stack trace."}}])))

(defn- stringify
  [thing]
  (if (or (map? thing) (string? thing) (number? thing) (sequential? thing))
    thing
    (str thing)))

(defn exception->json
  [exception {:keys [project-ns context group severity severity_reason unhandled user version environment meta] :as options}]
  (let [ex            (parse-exception exception)
        message       (:message ex)
        class-name    (.getName ^Class (:class ex))
        project-ns    (or project-ns "\000")
        stacktrace    (transform-stacktrace (:trace-elems ex) project-ns)
        base-meta     (if-let [d (ex-data exception)]
                        {"ex-data" d}
                        {})
        api-key       (impl/load-bugsnag-api-key! options)
        grouping-hash (or group
                          (if (isa? (type exception) clojure.lang.ExceptionInfo)
                            message
                            class-name))]
    {:apiKey   api-key
     :notifier {:name    "com.splashfinancial/clj-bugsnag"
                :version "1.1.0"
                :url     "https://github.com/SplashFinancial/clj-bugsnag"}
     :payloadVersion "4.0"
     :events   [{:exceptions     [{:errorClass class-name
                                   :message    message
                                   :stacktrace stacktrace}]
                 :breadcrumbs    []
                 :context        context
                 :groupingHash   grouping-hash
                 :severity       (or severity "error")
                 :severity_reason (or severity_reason {:type HANDLED_EXCEPTION})
                 :unhandled      (or unhandled false)
                 :user           user
                 :app            {:version      (or version (git-rev))
                                  :releaseStage (or environment "production")}
                 :device         {:hostname (get-hostname)}
                 :metaData       (walk/postwalk stringify (merge base-meta meta))}]}))

(defn notify
  "Notify Bugsnag about the supplied `exception`.
   By default, this function returns the HTTP response from Bugsnag.
   A second, optional argument may be passed to configure the behavior of the client.
   This map supports the following options.
     - :api-key - The BugSnag API key for your project.
                  If this key is missing, the library will attempt to load the Environment variable `BUGSNAG_KEY` and the JVM Property `bugsnagKey` in this order.
                  If all three values are nil, an exception will be thrown
     - :project-ns - The BugSnag project name you'd like to report the error to.
                     Typically the artifact name.
                     Defaults to \000
     - :context - The BugSnag 'context' in which an error occurred.
                  Defaults to nil.
                  See https://docs.bugsnag.com/platforms/java/other/customizing-error-reports/ for more details
     - :group - The BugSnag 'group' an error occurred within.
                Defaults to the exception message for instances of `clojure.lang.ExceptionInfo` or the Class Name of the Exception
     - :severity - The severity of the error.
                   Must be one of `info`, `warning`, and `error`.
                   Defaults to `error`
     - :severity_reason - Severity reason (used for Handled/Unhandled feature).
                          Defaults to {:type HANDLED_EXCEPTION}
     - :unhandled - Handled/Unhandled error flag, default to `false`
     - :user  - A string or map of facets representing the active end user when the error occurred.
                Defaults to nil
     - :version - The application version running when the error was reported.
                  Defaults to the git SHA when possible.
                  Otherwise nil.
     - :environment - The deployment context in which the error occurred.
                      Defaults to `Production`
     - :meta - A map of arbitrary metadata to associate to the error
     - :suppress-bugsnag-response? - A boolean toggle for this function's return value.
                                     When truthy, return nil- consistent with other logging interfaces and `println`
                                     When falsy, return the clj-http response from calling BugSnag's API
                                     Defaults to falsy."
  ([exception]
   (notify exception nil))

  ([exception {:keys [suppress-bugsnag-response?]
               :as   options}]
   (let [params (exception->json exception options)
         url    "https://notify.bugsnag.com/"
         resp   (http/post url {:form-params  params :content-type :json})]
     (if suppress-bugsnag-response?
       nil
       resp))))

(defn notify-v2!
  "Notify BugSnag about the supplied `exception`.
   By default, this function returns nil.
   A second, optional argument may be passed to configure the behavior of the client, which deviates in behavior from `notify`
   This map supports the following options.
     - :api-key - The BugSnag API key for your project.
                  If this key is missing, the library will attempt to load the Environment variable `BUGSNAG_KEY` and the JVM Property `bugsnagKey` in this order.
                  If all three values are nil, an exception will be thrown
     - :project-ns - The BugSnag project name you'd like to report the error to.
                     Typically the artifact name.
                     Defaults to \000
     - :context - The BugSnag 'context' in which an error occurred.
                  Defaults to nil.
                  See https://docs.bugsnag.com/platforms/java/other/customizing-error-reports/ for more details
     - :group - The BugSnag 'group' an error occurred within.
                Defaults to the exception message for instances of `clojure.lang.ExceptionInfo` or the Class Name of the Exception
     - :severity - The severity of the error.
                   Must be one of `info`, `warning`, and `error`.
                   Defaults to `error`
     - :user  - A string or map of facets representing the active end user when the error occurred.
                Defaults to nil
     - :version - The application version running when the error was reported.
                  Defaults to the git SHA when possible.
                  Otherwise nil.
     - :environment - The deployment context in which the error occurred.
                      Defaults to `Production`
     - :meta - A map of arbitrary metadata to associate to the error"
  ([exception]
   (notify-v2! exception nil))

  ([exception options]
   (let [params (exception->json exception options)
         url    "https://notify.bugsnag.com/"
         _      (http/post url {:form-params  params
                                :content-type :json})]
     nil)))
