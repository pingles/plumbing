(ns plumbing.prototype-test
  (:use clojure.test plumbing.prototype))


(deftest basic-test
  (let [o (-> (empty-obj)
	      (assoc :name "John") ; object data
	      (add-methods  ; add methods
	       { :name-lowered
		   (fn [this]
		     (.toLowerCase ^String (:name this)))
		:add-last-name
		  (fn [this last-name]
		    (str (:name this) " " last-name))}))]
    (is (= (:name o) "John"))  ; access obj data
    (is (= (o :name-lowered) "john")) ; object  method
    (is (= (o :add-last-name "Smith") "John Smith"))
    ;; test applyTo in IFn
    (is (= (apply o :add-last-name ["Smith"]) "John Smith"))))

(deftest mix-test
  (let [o1 (-> (empty-obj)
	       (assoc :a "1")
	       (add-methods
		  { :method1 (fn [this] "method1")}))
	o2 (-> (empty-obj)
	       (assoc :b "2")
	       (add-methods
		 {:method2 (fn [this] "method2")}))
	o (mix o1 o2)]
    (is (not (responds-to? o1 :method2)))
    (is (not (responds-to? o2 :method1)))
    (is (responds-to? o :method1))
    (is (= (:a o) "1"))
    (is (= (:b o) "2"))
    (is (responds-to? o :method2))))


