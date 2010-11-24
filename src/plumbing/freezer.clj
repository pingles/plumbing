(ns plumbing.freezer  
  {:doc "Serialization mechanism"
   :author "Aria Haghighi <me@aria42.com>"}
  (:use [clojure.contrib.duck-streams :only [copy, to-byte-array]]
        [clj-serializer.core :only [serialize deserialize]]))

(defprotocol Freezer
  (freeze [this o] "freeze object o into self")
  (thaw [this] "retrieve object"))

(defn to-bytes [o] (serialize o))

(defn from-bytes [^bytes bs]
  (deserialize bs :eof))

(extend-protocol Freezer

  java.io.File
  (freeze [this o]
      (with-open [in (java.io.ByteArrayInputStream. (to-bytes o))]
	(copy in this)))
  (thaw [this]
	(-> this to-byte-array from-bytes)))