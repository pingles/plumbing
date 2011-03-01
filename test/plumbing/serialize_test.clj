(ns plumbing.serialize-test
  (:use plumbing.serialize clojure.test))

(defn- round-trip-test [serialize-impl s]
  (is (= s (thaw serialize-impl (freeze serialize-impl s)))))

(defn serializer-test [serialize-impl]
  (doseq [s ["test1" {:a 1} [1]]]
    (round-trip-test serialize-impl s)))

(deftest default-serializer-test
  (serializer-test default-serializer))

(defn foo [] 1)

(deftest var-roundtrip
  (is (= 1
	 ((apply to-var (from-var #'foo)))))) 

(defn add [& args] (apply + args))

(deftest send-and-recieve-clj
  (is (= 6
	 (eval (recieve-clj (send-clj #'add 1 2 3))))))

(deftest send-and-recieve-json
  (is (= 6
	 (eval (recieve-json (send-json #'add 1 2 3))))))