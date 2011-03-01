(ns plumbing.serialize
  (:require [clj-json [core :as json]]
            [clojure.contrib.logging :as log])
  (:use	[clojure.contrib.def :only [defvar]]
	[plumbing.serialize]
	[plumbing.core :only [silent]])
  (:import clojure.lang.RT))

(defprotocol Serializer
  (freeze [this obj] "freeze obj to bytes")
  (thaw [this bytes] "convert bytes to object"))

(defrecord DefaultStringSerializer []
  Serializer
  (freeze [this obj] (.getBytes (prn-str obj)))
  (thaw [this bytes] (read-string (String. ^"[B" bytes))))

(def default-serializer   (DefaultStringSerializer.))

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
  (fn [msg] (recieve* (thaw serialize-impl msg))))

(def recieve-clj (mk-recieve-clj default-serializer))

(defn recieve-json
  "receive* message presented as a json string"
  [msg]
  (recieve* (json/parse-string msg)))

(defn mk-clj-worker
  "evaluate msg represented by serialized clojure object"
  [serialize-impl]
  (comp eval (mk-recieve-clj serialize-impl)))

(def clj-worker (mk-clj-worker default-serializer))

(defvar json-worker
  (comp eval recieve-json)
  "evaluate msg represented by json string")

(defn mk-send-clj
  "convert fn evaluation to String representing
   function evaluation as a clojure object message"
  [serialize-impl]
  (fn [fn-var & args]
    (freeze serialize-impl
     (-> fn-var
	 from-var
	 (cons args)))))

(defvar send-clj (mk-send-clj default-serializer)
  "default mk-send-clj with default serialization")

(defn send-json
  "convert fn evaluation to String json representation"
  [fn-var & args]
  (-> fn-var
      from-var
      (cons args)
      json/generate-string))