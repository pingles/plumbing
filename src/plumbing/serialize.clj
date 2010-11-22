(ns plumbing.freezer  
  {:doc "Serialization mechanism"
   :author "Aria Haghighi <me@aria42.com>"}
  (:use [clojure.contrib.duck-streams :only [copy, to-byte-array]]
        [clj-serializer.core :only [serialize deserialize]]))

(defprotocol Freezer
  (freeze [this o] "freeze object o into self")
  (thaw [this] "retrieve object"))

(extend-protocol Freezer

  java.io.File
  (freeze [this o]
	  (-> o serialize (copy this)))
  (thaw [this]
	(-> this to-byte-array (deserialize :eof)))

  String
  (freeze [this o]))