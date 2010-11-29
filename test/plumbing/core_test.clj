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
  (let [r (with-ex print-all /)]
  (is (= "{:ex \"java.lang.ArithmeticException: Divide by zero\", :stack (\"clojure.lang.Numbers.divide(Numbers.java:138)\" \"clojure.core$_SLASH_.invoke(core.clj:838)\" \"clojure.lang.AFn.applyToHelper(AFn.java:165)\" \"clojure.lang.RestFn.applyTo(RestFn.java:133)\" \"clojure.core$apply.invoke(core.clj:540)\" \"plumbing.core$with_ex$fn__550.doInvoke(core.clj:196)\" \"clojure.lang.RestFn.invoke(RestFn.java:422)\" \"plumbing.core_test$fn__642.invoke(core_test.clj:73)\" \"clojure.test$test_var$fn__6131.invoke(test.clj:688)\" \"clojure.test$test_var.invoke(test.clj:688)\" \"clojure.test$test_all_vars$fn__6135$fn__6142.invoke(test.clj:704)\" \"clojure.test$default_fixture.invoke(test.clj:658)\" \"clojure.test$test_all_vars$fn__6135.invoke(test.clj:704)\" \"clojure.test$default_fixture.invoke(test.clj:658)\" \"clojure.test$test_all_vars.invoke(test.clj:700)\" \"clojure.test$test_ns.invoke(test.clj:723)\" \"clojure.core$map$fn__3695.invoke(core.clj:2096)\" \"clojure.lang.LazySeq.sval(LazySeq.java:42)\" \"clojure.lang.LazySeq.seq(LazySeq.java:56)\" \"clojure.lang.Cons.next(Cons.java:39)\" \"clojure.lang.RT.boundedLength(RT.java:1186)\" \"clojure.lang.RestFn.applyTo(RestFn.java:131)\" \"clojure.core$apply.invoke(core.clj:542)\" \"clojure.test$run_tests.doInvoke(test.clj:738)\" \"clojure.lang.RestFn.applyTo(RestFn.java:138)\" \"clojure.core$apply.invoke(core.clj:540)\" \"user$eval671.invoke(NO_SOURCE_FILE:1)\" \"clojure.lang.Compiler.eval(Compiler.java:5424)\" \"clojure.lang.Compiler.eval(Compiler.java:5414)\" \"clojure.lang.Compiler.eval(Compiler.java:5415)\" \"clojure.lang.Compiler.eval(Compiler.java:5391)\" \"clojure.core$eval.invoke(core.clj:2382)\" \"clojure.main$eval_opt.invoke(main.clj:235)\" \"clojure.main$initialize.invoke(main.clj:254)\" \"clojure.main$null_opt.invoke(main.clj:279)\" \"clojure.main$main.doInvoke(main.clj:354)\" \"clojure.lang.RestFn.invoke(RestFn.java:458)\" \"clojure.lang.Var.invoke(Var.java:377)\" \"clojure.lang.AFn.applyToHelper(AFn.java:174)\" \"clojure.lang.Var.applyTo(Var.java:482)\" \"clojure.main.main(main.java:37)\"), :fn {:ns \"clojure.core\", :name \"/\"}, :args (\"4\" \"0\")}"
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