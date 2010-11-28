(ns plumbing.core-test
  (:use [plumbing core]
	[clojure test]))

(deftest map-map-test
  (is (= (map-map inc {:a 0 :b 0})
	 {:a 1 :b 1})))

(deftest map-keys-test
  (is (= (map-keys str {:a 1 :b 1})
	 {":a" 1 ":b" 1})))

(deftest map-from-keys-test
  (is (= (map-from-keys inc [0 1 2])
	 {0 1, 1 2, 2 3})))

(deftest set-to-unit-map-test
  (is (= (set-to-unit-map #{1 2 3})
	 {1 1 2 1 3 1})))

(deftest or-else-test
  (are (= (or-else nil 1) 1)
       (= (or-else 1 2) 1)))

(deftest -?>-test 
  (are (-?> {:a 1} :a inc) 2
       (-?> {:a 1} inc) nil))

(deftest -?>-test 
    (are (-?>> [1 2] (map inc)) [2 3]
	 (-?>> {:a 1} inc) nil))

(deftest successfull
  (is (= 10
    (retry 5 + 4 6))))

(deftest failure
  (is (= :fail
    (retry 5 / 4 0))))
