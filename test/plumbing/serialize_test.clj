(ns plumbing.serialize-test
  (:use plumbing.serialize
	clojure.test)
  (:import (org.apache.commons.io IOUtils)
           (java.io ByteArrayOutputStream)))

(deftest read-ln-test
  (is (= "GET"
         (read-ln (IOUtils/toInputStream "GET\r\n")))))

(deftest read-arg-count-test
  (is (= 2
         (read-arg-count (IOUtils/toInputStream
                          "*2\r\n$3\r\nGET\r\n$3\r\nkey\r\n")))))

(deftest read-arg-len-test
  (is (= 3
         (read-arg-len (IOUtils/toInputStream
                        "$3\r\nGET\r\n$3\r\nkey\r\n")))))

(deftest read-arg-test
  (is (= (seq "GET")
         (seq (read-arg (IOUtils/toInputStream
                         "$5\r\n\"GET\"\r\n$3\r\nkey\r\n"))))))

(deftest read-msg-test
  (let [[cmd & args] (read-msg (IOUtils/toInputStream
                                "*2\r\n$5\r\n\"GET\"\r\n$5\r\n\"key\"\r\n"))]
    (is (= "GET" cmd))
    (is (= (list "key")
           args))))

(deftest write-arg-count-test
  (let [baos (doto (ByteArrayOutputStream.)
               (write-arg-count 20))]
    (is (= "*20\r\n"
           (String. (.toByteArray baos))))))

(deftest write-arg-test
  (let [baos (doto (ByteArrayOutputStream.)
               (write-arg (pr-str 1024)))]
    (is (= "$4\r\n1024\r\n"
           (String. (.toByteArray baos))))))

(deftest write-msg-test
  (let [baos (doto (ByteArrayOutputStream.)
               (write-msg [555 "rawr"]))]
    (is (= "*2\r\n$3\r\n555\r\n$6\r\n\"rawr\"\r\n"
           (String. (.toByteArray baos))))))

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