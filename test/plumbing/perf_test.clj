(ns plumbing.perf-test
  (:use plumbing.serialize))

(defn benchmark-println-messaging [xs]
  (let [out (java.io.ByteArrayOutputStream.)]
    (time
     (do
       (doall (map (partial write-str-msg (writer out)) xs))
       (let [in (reader (java.io.ByteArrayInputStream.
			 (.toByteArray out)))]
	 (repeatedly (count xs)
		     #(read-str-msg in)))))))

(defn benchmark-protocol-messaging [xs]
  (let [out  (java.io.ByteArrayOutputStream.)]
    (time
     (do
       (doall (map (partial write-msg out) xs))
       (.flush out)
       (let [in (java.io.ByteArrayInputStream. (.toByteArray out))]
	 (repeatedly (count xs)
		     #(read-msg in)))))))