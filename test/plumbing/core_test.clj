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
	
(deftest logging
  (let [[a l] (atom-logger)
	f (with-ex l /)
	_ (f 4 0)]
  (is (= "java.lang.ArithmeticException: Divide by zero" @a))))

(deftest time-out
  (let [[a l] (atom-logger)
	f (with-ex l
	    (with-timeout 1 #(Thread/sleep 10000)))
	_ (f)]
  (is (= "java.util.concurrent.TimeoutException" @a))))

(defn fake-http-request [n result]
  (let [retries (atom 0)] 
    (fn [u]
      (if (= n @retries)
	result
	(do
	  (swap! retries inc)
	  (throw (java.lang.RuntimeException. "foo")))))))

(deftest successfull
  (is (= 10
    (retry 5 + 4 6))))

(deftest failure
  (let [[a l] (atom-logger)
	r (with-ex l
	     (with-retries 5 /))
        rs (with-silent
	     (with-retries 5 /))]  
  (is (= "java.lang.Exception: Retry Exception."
    (r 4 0)))
  (is (= nil
    (rs 4 0)))))

(deftest with-print-all
  (let [r (with-ex (print-all :ex :fn :args) /)]
  (is (= "{:ex \"java.lang.ArithmeticException: Divide by zero\", :fn {:ns \"clojure.core\", :name \"/\"}, :args (\"4\" \"0\")}"
    (r 4 0)))))

(deftest http-retries
  (let [r1 (with-retries 5 (fake-http-request 2 "got it"))
	[a l] (atom-logger)
	r2 (with-ex l
	     (with-retries 5 (fake-http-request 6 "got it")))]
  (is (= "got it"
	 (r1 "http://fake.ass.url")))
  (is (= "java.lang.Exception: Retry Exception."
	 (r2 "http://fake.ass.url")))))

(deftest pipeline-compose
  (is (= 10
	 (-->> [3 4 2 1] + identity identity))))

(deftest default-compose
  (is (= 10
	 (-x> 10 (/ 0)))))

(deftest silent-nil
  (is (= [nil 3 4]
	 (map (with-silent inc) ["1" 2 3]))))