(ns plumbing.serialize-test
  (:use plumbing.serialize clojure.test))

(defn- round-trip-test [serialize-impl s]
  (is (= s (thaw serialize-impl (freeze serialize-impl s)))))

(defn serializer-test [serialize-impl]
  (doseq [s ["test1" {:a 1} [1]]]
    (round-trip-test serialize-impl s)))

(deftest default-serializer-test
  (serializer-test default-serializer))

