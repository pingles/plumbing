(ns plumbing.core
  {:doc "General purpose functions"
   :author "Aria Haghighi <me@aria42.com>. Many fns lifted
   from existing clj-sys and other projects."}
  (:require [clojure.contrib.logging :as log]))

;;
;;  map functions
;;

(defn map-map
  "returns map k -> (f v) for [k v] in map m"
  [f m]
  (into {} (for [kv m] [(first kv) (f (second kv))])))

(defn map-keys
  "returns map (f k) -> v for [k v] in map m"
  [f m]
  (into {} (for [kv m] [(f (first kv)) (second kv)])))

(defn map-from-keys
  "returns map k -> (f k) for keys in ks"
  [f ks]
  (into {} (for [k ks] [k (f k)])))

(defn map-from-pairs
  "returns map k -> (f k v) for [k v] in kvs"
  [f kvs]
  (into {} (for [[k v] kvs] [k (f k v)])))

(defn map-from-nested-map
  "returns map k -> (map-map f v) from [k v] from m"
  [f m]
  (into {} (for [[k v] m] [k (map-map f v)])))

(defn set-to-unit-map
  "returns map k -> 1 for k in set s"
  [s]
  (into {} (for [x s] [x 1])))

(defmacro map->
  "performs map with argument inserted after fn in form"
  [form aseq]
  `(map #(~(first form) % ~@(rest form)) ~aseq))

(defmacro map->>
  "performs map with argument insterted at end of form"
  [form aseq]
  `(map #(~(first form) ~@(rest form) %) ~aseq))

(defmacro reducate
  "map:for :: reduce:reducate. lifted
   from https://github.com/Seajure/reducate"
  ([accum bindings body]
     `(reduce (fn ~[(first accum) (first bindings)] ~body)
              ~(second accum) ~(second bindings)))
  ([[name value] body]
     `(let [[init# & more#] ~value]
        (reducate [~'% init#] [~name more#] ~body))))

;;
;; General 
;;

(defn sum
  "returns sum (f x) for x in xs"
  ([f xs]
     (reduce
      (fn [res x] (+ res (f x)))
      0.0 xs))
  ([xs] (reduce + xs)))

(defn rpartial
  "right curry args, might be inefficient"
  ([f & args]
     (fn [& prefix-args]
       (apply f (concat prefix-args args)))))

(defn rcomp
  "composes function f1 f2 f3 in reverse order
   so (f3 (f2 (f1 x))), when you process data
   you can think of the functions as happening
   in order"
  [& fs]
  (apply comp (reverse fs)))

;;
;;  error handling defaults 
;;

(defn or-else
  "returns default value if x is nil"
  [x default]
  (if x x default))

(defmacro try-silent
  "evaluates a form, returning nil if evaluating the form throws an exception." 
  [f]
  `(try ~f (catch Exception e# nil)))

(defn try-update
  "Tries to perform update function f on (apply f x args)
  returning x if update throws exception or returns nil"
  [x f & args]
  (try (or (apply f x args) x)
       (catch Exception _ x)))

;; (defmacro try-log
;;   "like try-silent except logs msg with level (defaults to :error)
;;    on exception, returning nil"
;;   [f & [msg level]]
;;   `(try ~f
;; 	(catch Exception e#
;; 	  (log/log (or ~level :error)   )
;; 	  (log/log (or ~level :error) msg)
;; 	  nil)))

(defn maybe-comp
  "returns composition of fns as in comp, but
   returning nil when a given value return"
  [& fs]
  (fn [x]
    (try-silent
     (reduce
      #(if (not %1) %1 (%2 %1))
      x (reverse fs)))))

(defmacro -?>
  "first position threaded operator which short-circuits
   on nil or on exception and returns nil in that case"
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta `(try-silent (~(first form) ~x ~@(next form))) (meta form))
              `(try-silent (~form ~x))))
  ([x form & more] `(when-let [f# (-?> ~x ~form)] (-?> f# ~@more))))

(defmacro -?>>
  "last position threaded operator which short-circuits
   on nil or on exception and returns nil in that case"
  ([x] x)
  ([x form] (if (seq? form)
              (with-meta `(try-silent (~(first form) ~@(next form)  ~x)) (meta form))
              `(try-silent (~form ~x))))
  ([x form & more] `(when-let [f# (-?>> ~x ~form)] (-?>> f# ~@more) )))

;; Core Protocols

(defprotocol ToSeqable
  (to-seq [this] "the protocol version of seqable, temporary
                  until clojure makes Seqable a protocol"))

(extend-protocol ToSeqable
  clojure.lang.Seqable
  (to-seq [this] (seq this)))

(defn find-first
  ([f coll]
     (first (filter f coll)))
  ([coll] (find-first identity coll)))

;; Control
(defn retry [retries f & args]
  "Retries applying f to args based on the number of retries.
  catches generic Exception, so if you want to do things with other exceptions, you must do so in the client code inside f."
  (try (apply f args)
    (catch java.lang.Exception _
      (if (zero? retries)
;;TODO: decide on c.c.condition or roll our own.  throw a proper new RetryException wrapping the nested exception.
;;http://groups.google.com/group/clojure-dev/browse_thread/thread/734ee59f6cbc1b55
;;http://dev.clojure.org/display/design/Exception+Handling
	(throw (java.lang.Exception. "Retry Exception."))
        (apply retry (- retries 1) f args)))))

(defn with-timeout
  "tries to execute (apply f args)
   in secs, and throws TimeOut exception
   if it fails. Can capture exception
   with try-silent"
  [secs f]
  (fn  [& args]
    (let [f (future (apply f args))]
      (.get f (long (* secs 1000))
	    (java.util.concurrent.TimeUnit/MILLISECONDS)))))

(defn with-retries [retries f]
  "Retries applying f to args based on the number of retries.
  catches generic Exception, so if you want to do things with other exceptions, you must do so in the client code inside f.
if the last retry fails, rethrows."
  (fn [& args]
    (apply retry retries f args)))

(defn with-ex [h f]
"takes a handler h and a function f."
  (fn  [& args]
    (try
     (apply f args)
     (catch java.lang.Exception e
       (h e f args)))))

(defn with-silent [f]
 (with-ex (constantly nil) f))

(defmacro -->> [args f & wrappers]
  `(apply (->> ~f ~@wrappers) ~args))

(defn set-log-level! [level]
  (case log/*impl-name*
	(-> (org.apache.log4j.Logger/getRootLogger)
	    (.setLevel
	      (case level
		    :all org.apache.log4j.Level/ALL
		    :trace org.apache.log4j.Level/TRACE
		    :debug org.apache.log4j.Level/DEBUG
		    :info  org.apache.log4j.Level/INFO
		    :warn  org.apache.log4j.Level/WARN
		    :error org.apache.log4j.Level/ERROR
		    :fatal org.apache.log4j.Level/FATAL)))))

(defn atom-logger []
  (let [a (atom "")
	l (fn [e & args] (swap! a (fn [x] (str e))))]
    [a l]))

;;TODO: we should probably smartly filter the stack traces, and we should also probably not print them all the time, as the logs may blow up to be huge, not sure.
(defn print-all [e f args]
  (pr-str {:ex (str e)
	   :stack (map str (.getStackTrace e))
	   :fn (let [m (meta f)]
		       {:ns (str (:ns m))
			:name (str (:name m))})
	   :args (map pr-str args)}))

;;TODO: can test with a log appender fake.
;;http://www.mail-archive.com/log4j-user@logging.apache.org/msg08646.html
(defn logger [& [level]]
  (fn [e f args]
    (let [l (or level :debug)
	  m (print-all e f args)]
      (log/log l m))))

(defn with-log [f & [level]]
 (with-ex (logger level) f))