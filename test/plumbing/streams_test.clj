(ns plumbing.streams-test
  (:use clojure.test
	plumbing.streams))

(deftest generating-a-seq
  (let [[g1 f1] (lazy-seq-of-seqs [[1] [2] [3]])
	[g2 f2] (f1)
	[g3 f3] (f2)]
    (is (= 1 g1))
    (is (= 2 g2))
    (is (= 3 g3))
    (is (= nil (f3)))))

(deftest lazy-flatten-test
  (is (= [1 2 3]
	   (iterator-seq (flat-iter [[1] [2] [3]])))))