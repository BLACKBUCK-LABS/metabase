(ns expectations
  (:require [clojure
             [data :as data]
             [test :as t]]
            [clojure.spec.alpha :as spec]
            [expectations.clojure.test :as ex.test]))

(defn- spec? [e]
  (and
   (keyword? e)
   (boolean (spec/get-spec e))))

(defn- compare-expr [expected actual]
  (cond
    (spec? expected)
    (spec/valid? expected actual)

    (map? expected)
    (= (into {} expected)
       (into {} actual))

    (fn? expected)
    (expected actual)

    :else
    (= expected actual)))

(defn compare* [expected actual message [_ e :as form]]
  (let [result (compare-expr expected actual)]
    (t/do-report
     (if result
       {:type     :pass
        :message  message
        :expected form
        :actual   (if (fn? expected)
                    (list e actual)
                    actual)}
       {:type     :fail
        :message  (if (spec? expected)
                    (spec/explain-str expected)
                    message)
        :diffs    [[actual (take 2 (data/diff expected actual))]]
        :expected expected
        :actual   (if (fn? expected)
                    (list 'not (list e actual))
                    [actual])}))))

;; tweaked replacement of the version that ships with `expectations.clojure.test` -- this one replicates some of our
;; custom tweaks like considering a regular map and a record type (usually a Toucan instance) to be equal if their
;; contents are equal
(defmethod t/assert-expr '=? [msg form]
  ;; (is (=? val-or-pred expr))
  (let [[_ e a] form]
    `(let [e# ~e
           a# ~a]
       (compare* e# a# ~msg '~form))))

(defmacro ^:deprecated expect
  "Simple macro that simulates converts an Expectations-style `expect` form into a `clojure.test` `deftest` form."
  {:arglists '([actual] [actual expected])}
  [& args]
  `(t/deftest ~(symbol (format "expect-%d" (hash &form)))
     (ex.test/expect ~@args)))
