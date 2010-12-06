(ns plumbing.serialize)

(defprotocol Serializer
  (freeze [this obj] "freeze obj to bytes")
  (thaw [this bytes] "convert bytes to object"))

(defrecord DefaultStringSerializer []
  Serializer
  (freeze [this obj] (.getBytes (prn-str obj)))
  (thaw [this bytes] (read-string (String. ^"[B" bytes))))

(def default-serializer   (DefaultStringSerializer.))

