(ns plumbing.serialize
  (:require [clj-json [core :as json]]
            [clojure.contrib.logging :as log])
  (:use	[clojure.contrib.def :only [defvar]]
	[clojure.string :only [lower-case]]
	[plumbing.serialize]
	[plumbing.core :only [silent]])
  (:import clojure.lang.RT
	   (java.nio ByteBuffer)
	   (java.util Arrays)
	   (java.io InputStream OutputStream
		    BufferedReader InputStreamReader
		    PrintWriter)))

(defprotocol Serializer
  "A protocol to convert between Clojure values
   and byte arrays."
  (^bytes serialize [this x])
  (deserialize [this ^bytes bs]))

(defn string-serializer
  [& {:keys [encoding]
      :or {encoding "UTF8"}}]
  (reify Serializer
         (serialize [this x]
                    (.getBytes (pr-str x) encoding))
         (deserialize [this bs]
                      (read-string (String. ^"[B" bs encoding)))))

(defn read-str-msg [^InputStream in]
  (-> in
      (InputStreamReader.)
      (BufferedReader.)
      (.readLine)
      read-string))

(defn write-str-msg [^OutputStream out msg]
  (-> out
      (PrintWriter.)
      (.println (pr-str msg))))

(defn cr? [x]
  (= 13 x))

(defn nl? [x]
  (= 10 x))

(defn ubyte [x]
  (if (> x 127)
    (byte (- x 256))
    (byte x)))

(defn as-int [^Byte b]
  (bit-and b 0xFF))

(defn ^String read-ln [^InputStream is]
  (let [bb (ByteBuffer/allocate 32)]
    (loop [last-val 0]
      (let [cur-val (as-int (.read is))]
        (if (and (cr? last-val)
                 (nl? cur-val))
          (String. (Arrays/copyOf (.array bb)
                                  (- (.position bb) 1)))
          (do
            (.put bb (ubyte cur-val))
            (recur cur-val)))))))

(defn pr-java [x]
  (cond
   (instance? java.lang.Enum x) (str x)
   :default x))

(defn arg-count? [^String x]
  (.startsWith x "*"))

(defn arg-len? [^String x]
  (.startsWith x "$"))

(defn read-arg-count [^InputStream is]
  (let [l (read-ln is)]
    (if (arg-count? l)
      (Integer/parseInt (.substring l 1))
      (throw (Exception. "Error parsing arg count.")))))

(defn read-arg-len [^InputStream is]
  (let [l (read-ln is)]
    (if (arg-len? l)
      (Integer/parseInt (.substring l 1))
      (throw (Exception. "Error parsing arg length.")))))

(defn read-arg [^InputStream is]
  (let [arg-len (read-arg-len is)
        buf (byte-array arg-len)]
    (.read is buf)
    (.skip is 2)
    (read-string (String. buf))))

(defn read-msg [^InputStream is]
  (let [arg-count (read-arg-count is)
        args (repeatedly arg-count #(read-arg is))]
    args))

(defn write-arg-count [^OutputStream os c]
  (.write os (.getBytes
              (format "*%d\r\n" c))))

(defn write-arg [^OutputStream os ^String arg]
  (let [arg-len (.length arg)]        
    (.write os (.getBytes
                (format "$%d\r\n%s\r\n"
                        arg-len arg)))))

;; TODO: Move pr-java out into own composable fn
(defn write-msg [^OutputStream os args]
  (let [num-args (count args)]
    (write-arg-count os num-args)
    (doseq [arg args] (write-arg os ((comp pr-str pr-java) arg)))))

(defn from-var
  "convert fn variable to [ns-name fn-name] string pair"
  [^Var fn-var]
  (let [m (meta fn-var)]
    [(str (:ns m)) (str (:name m))]))

(defn to-var
  "find variable named by [ns-name fn-name] strings"
  [^String ns-name ^String fn-name]
  (let [root (-> ns-name
		 (.replace "-" "_")
		 (.replace "." "/"))]
    (silent #(RT/load %1) root)
    (.deref (RT/var ns-name, fn-name))))

(defn- recieve*
  "msg should take form [[ns-name fn-name] args]
   and return a list which when eval'd represents
   executing fn on args" 
  [msg]
  (let [[[ns-name fn-name] & args] msg]
    (cons (to-var ns-name fn-name) args)))

(defn mk-recieve-clj
  "receive* message represented as a serialized
   clojure data object"
  [serialize-impl]
  (fn [msg] (recieve* (deserialize serialize-impl msg))))

(def recieve-clj (mk-recieve-clj (string-serializer)))

(defn recieve-json
  "receive* message presented as a json string"
  [msg]
  (recieve* (json/parse-string msg)))

(defn mk-clj-worker
  "evaluate msg represented by serialized clojure object"
  [serialize-impl]
  (comp eval (mk-recieve-clj serialize-impl)))

(def clj-worker (mk-clj-worker (string-serializer)))

(defvar json-worker
  (comp eval recieve-json)
  "evaluate msg represented by json string")

(defn mk-send-clj
  "convert fn evaluation to String representing
   function evaluation as a clojure object message"
  [serialize-impl]
  (fn [fn-var & args]
    (serialize serialize-impl
     (-> fn-var
	 from-var
	 (cons args)))))

(defvar send-clj (mk-send-clj (string-serializer))
  "default mk-send-clj with default serialization")

(defn send-json
  "convert fn evaluation to String json representation"
  [fn-var & args]
  (-> fn-var
      from-var
      (cons args)
      json/generate-string))