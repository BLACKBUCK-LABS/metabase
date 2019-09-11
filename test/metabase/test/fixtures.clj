(ns metabase.test.fixtures
  (:require [metabase.test.initialize :as initialize]))

(defn initialze [& what]
  (fn [f]
    (apply initialize/initialize-if-needed! what)
    (f)))
