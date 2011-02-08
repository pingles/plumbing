(ns plumbing.classpath-test
  (:use clojure.test
	plumbing.classpath))

(deftest get-the-project-root
  (is (.contains 
       (parent-path "plumbing.classpath")
       "plumbing")))