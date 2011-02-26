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

(deftest wrap-copy-test
  (let [to-zero (int-array 10)]
	 (wrap-copy (int-array [1 2 3])
		    0 10 to-zero 0)
	 (is (= [1 2 3 1 2 3 1 2 3 1] (seq to-zero))))
  (let [to-two (int-array 10)]
	 (wrap-copy (int-array [1 2 3])
		    2 10 to-two 0)
	 (is (= [3 1 2 3 1 2 3 1 2 3] (seq to-two)))))

#_(deftest test-stream-test
  (let [ts (test-stream (.getBytes "ballsdeep") 10 (.getBytes "4"))
	howdeep? (byte-array 9)
	bigread (byte-array 18)]
    (.read ts howdeep?)
    (is (= "ballsdeep"
	   (String. howdeep?)))
    (.read ts bigread)
    (is (= "ballsdeepballsdeep"
	   (String. bigread)))))

(deftest available-stream-test
  (let [ts (test-stream
	    (.getBytes "ballsdeep") 1
	    (.getBytes "0"))
	howdeep? (byte-array 9)]
    (is (= 10
	   (.available ts)))))

;; (deftest read-eof-stream-test
;;   (let [ts (test-stream
;; 	    (.getBytes "ballsdeep") 1
;; 	    (.getBytes "0\n"))
;; 	howdeep? (byte-array 11)
;; 	nothing (byte-array 1)]
;;     (.read ts howdeep?)
;;     (is (= (str "ballsdeep0\n")
;; 	   (String. howdeep?)))
;;     (.read ts nothing)
;;     (is (= "\0"
;; 	   (String. nothing)))))

#_(deftest readline-eof-stream-test
  (let [ts (-> (test-stream
		   (.getBytes "ballsdeep\nballsdeep") 1
		   (.getBytes "0\n"))
	       java.io.InputStreamReader.
	       java.io.BufferedReader.)]
    (is (= "ballsdeep"
	   (.readLine ts)))
    (is (= (str "ballsdeep0\n")
	   (.readLine ts)))))