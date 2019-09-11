(ns metabase.test.initialize
  "Logic for initializing different components that needed to be initialized when running tests."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [colorize.core :as colorize]
            [metabase
             [db :as mdb]
             [handler :as handler]
             [plugins :as plugins]
             [server :as server]
             [task :as task]
             [util :as u]]
            [metabase.core.initialization-status :as init-status]
            [metabase.models.setting :as setting]
            [metabase.plugins.initialize :as plugins.init]
            [metabase.test.data.env :as tx.env]
            [yaml.core :as yaml]))

(defmulti initialize-if-needed!
  "Initialize one or more components.

    (initialize-if-needed! :db :web-server)"
  (fn
    ([k]        (keyword k))
    ([k & more] :many)))

(defmethod initialize-if-needed! :many
  [& args]
  (doseq [k args]
    (initialize-if-needed! k)))

(defn- driver-plugin-manifest [driver]
  (let [manifest (io/file (format "modules/drivers/%s/resources/metabase-plugin.yaml" (name driver)))]
    (when (.exists manifest)
      (yaml/parse-string (slurp manifest)))))

(defn- driver-parents [driver]
  (let [parents-file (io/file (format "modules/drivers/%s/parents" (name driver)))]
    (when (.exists parents-file)
      (str/split-lines (slurp parents-file)))))

(defn- load-plugin-manifests!
  "When running tests driver plugins aren't loaded the normal way -- instead, to keep things sane, we simply merge their
  dependencies and source paths into the Metabase core project via a custom Leiningen plugin. We still need to run
  appropriate plugin initialization code, however, in order to ensure the drivers do things like register proxy
  drivers or get methods for `connection-properties`.

  Work some magic and find manifest files and load them the way the plugins namespace would have done."
  ([]
   (load-plugin-manifests! tx.env/test-drivers))

  ([drivers]
   (doseq [driver drivers
           :let   [info (driver-plugin-manifest driver)]
           :when  info]
     (println (u/format-color 'green "Loading plugin manifest for driver as if it were a real plugin: %s" driver))
     (plugins.init/init-plugin-with-info! info)
     ;; ok, now we need to make sure we load any depenencies for those drivers as well (!)
     (load-plugin-manifests! (driver-parents driver)))))

(defn- task-name-init-message [task-name]
  (let [body   (format "| Initializing %s... |" task-name)
        border (str \+ (str/join (repeat (- (count body) 2) \-)) \+)]
    (str "\n"
         (str/join "\n" [border body border])
         "\n")))

(defmacro ^:private define-initialization [task-name & body]
  (let [delay-symb (vary-meta (symbol (format "init-%s-%d" (name task-name) (hash &form)))
                              assoc :private true)]
    `(do
       (defonce ~delay-symb
         (delay
           (println (colorize/blue ~(task-name-init-message task-name)))
           ~@body
           nil))
       (defmethod initialize-if-needed! ~(keyword task-name)
         [~'_]
         @~delay-symb))))

(define-initialization :plugins
  (plugins/load-plugins!)
  (load-plugin-manifests!))

(define-initialization :scheduler
  ;; we don't want to actually start the task scheduler (we don't want sync or other stuff happening in the BG
  ;; while running tests), but we still need to make sure it sets itself up properly so tasks can get scheduled
  ;; without throwing Exceptions
  (#'task/set-jdbc-backend-properties!))

(define-initialization :web-server
  (try
    (server/start-web-server! #'handler/app)
    (catch Throwable e
      (log/error e "Web server failed to start")
      (System/exit -2)))
  (init-status/set-complete!)
  (setting/set! :site-name "Metabase Test"))

(define-initialization :db
  (println (format "Setting up %s test DB and running migrations..." (mdb/db-type)))
  (mdb/setup-db!))
