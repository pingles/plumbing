(ns plumbing.core
  (:require [clojure.contrib.logging :as log])
  (:use [clojure.contrib.pprint :only [pprint]]))

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

(defn map-from-vals
  "returns map (f v) -> v for vals in vs"
  [f vs]
  (into {} (for [v vs] [(f v) v])))

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

(defn keywordize-map
  "recursively convert maps in m (including itself)
   to have keyword keys instead of string"
  [m]
  (cond
   (instance? clojure.lang.IPersistentMap m)
     (into {}
      (map 
       (fn [[k v]]
	 [(if (string? k) (keyword k) k) (keywordize-map v)])
       m))
   (instance? clojure.lang.IPersistentList m)
     (map keywordize-map m)
   (instance? clojure.lang.IPersistentVector m)
     (into [] (map keywordize-map m))
   :else m))

(defn with-accumulator
  ([f init-val]
     (let [a (atom init-val)]
       [(fn [x] (swap! a f x)) a]))
  ([f] (with-accumulator f nil)))

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

(defn unique-by
  "returns elements of xs which return unique
   values according to function f. The representative
   of each f value is the first element in xs with that value"
  [f xs]
  (second
   (reduce
      (fn [[ids res] x]
	(let [id (f x)]
	  (if (ids id)
	    [ids res]
	    [(conj ids id) (conj res x)])))
      [#{} nil]
      xs)))

;;
;; Maps
;;

(defn update-by
  "Returns a new map where the value of all ks
  in map are replaced with (f v)."
  [m f k & ks]
  (let [ks (cons k ks)]
    (merge m (zipmap ks
                     (map (fn [k] (f (get m k))) ks)))))


;;; Finding things

(defn find-top-k
  "return top k members of elem
   according to function f. At no point
   stores more than k elements in memory
   at a time."
  [f k elems]
  (->> elems
       (map (juxt f identity))
       (reduce
        (fn [top score-elem]          
          (->>  (conj top score-elem)
                (sort-by (comp - first))
                (take k)))
        nil)
       (map second)))
;;
;;  error handling defaults 
;;


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
    (catch java.lang.Exception e
      (if (zero? retries)
	(throw e)
        (apply retry (- retries 1) f args)))))

(defn wait [secs f & args]
  "Wait until either the given number of seconds has transpired or
  once f returns a non nil value."
  (let [start-time (System/currentTimeMillis)]
    (loop []
      (let [r (apply f args)
            curr-time (System/currentTimeMillis)
            secs-count (/ (- curr-time start-time) 1000)]
        (cond (and (nil? r) (< secs-count secs)) (do (Thread/sleep 5000)
                                                     (recur))
              (not (nil? r)) r
              (>= secs-count secs) (throw (java.lang.Exception. "Wait Exception.")))))))

(defn wait-until
  ([cond-f]
     (loop []
       (if (cond-f)
         nil
         (do (Thread/sleep 1000)
             (recur)))))
  ([cond-f secs]
     (loop [secs-left secs]
       (if (or (cond-f) (<= secs-left 0))
         nil
         (do (Thread/sleep 1000)
             (recur (dec secs-left)))))))

(defn silent [f & args]
  (try
    (apply f args)
    (catch Exception _ nil)))

(defn with-obs [o f]
  (fn [& args]
    (o f args)
    (apply f args)))


(defn with-timeout
  "tries to execute (apply f args)
   in secs, and cancels future if it
   fails to complete in time. The Timeout Exception is still thrown
   so it needs to be captured via with-ex, try-silent, etc. Other exceptions
   are simply thrown."
  [secs f]
  (fn [& args]
    (let [f (future (apply f args))]
      (try (.get f
                 (long secs)
                 (java.util.concurrent.TimeUnit/SECONDS))
           (catch java.util.concurrent.TimeoutException e
             (.cancel f true)
             (throw e))))))

(defn with-retries [retries f]
  "Retries applying f to args based on the number of retries.
  catches generic Exception, so if you want to do things with other exceptions, you must do so in the client code inside f.
if the last retry fails, rethrows."
  (fn [& args]
    (apply retry retries f args)))

(defn with-wait [secs f]
  "Retries applying f to args until a non-nil value is returned or the number
  of seconds"
  (fn [& args]
    (apply wait secs f args)))

(defn cause [e]
  (if-let [c (.getCause e)]
    (recur c)
    (.getName (class e))))

(defn with-ex [h f]
"takes a handler h and a function f."
  (fn [& args]
    (try
     (apply f args)
     (catch java.lang.Exception e
       (h e f args)))))

(defn with-silent [f]
  (with-ex (fn [& args] nil) f))

(defn maybe-comp
  "returns composition of fns as in comp, but
   returning nil when a given value return"
  [& fs]
  (fn [x]
    ((with-silent reduce)
     #(if (not %1) %1 (%2 %1))
     x (reverse fs))))

(defmacro -?>
  "first position threaded operator which short-circuits
   on nil or on exception and returns nil in that case"
  ([x] x)
  ([x form] (if (seq? form) 
              (with-meta
		`(try
		   (~(first form) ~x ~@(next form))
		   (catch Exception e# nil))
		(meta form))
		`(try
		   (~form ~x)
		   (catch Exception e# nil))))
  ([x form & more] `(when-let [f# (-?> ~x ~form)] (-?> f# ~@more))))

(defmacro -->> [args f & wrappers]
  `(apply (->> ~f ~@wrappers) ~args))

(defmacro -log>
  [x & args]
  `(try
     (-> ~x ~@args)
     (catch Exception e#
       ((logger) e# nil nil))))

(defmacro -x>
  [x & args]
  `(try (or (-> ~x ~@args) ~x)
        (catch Exception _# ~x)))

(defn set-log-level!
  ([level]
     (set-log-level! [(org.apache.log4j.Logger/getRootLogger)] level))
  ([loggers level]
     (let [loggers (map (fn [l] (if (string? l)
                                  (org.apache.log4j.Logger/getLogger l)
                                  l))
                        loggers)]
       (doseq [l loggers]
         (.setLevel l (case level
                            :all org.apache.log4j.Level/ALL
                            :debug org.apache.log4j.Level/DEBUG
                            :error org.apache.log4j.Level/ERROR
                            :fatal org.apache.log4j.Level/FATAL
                            :info org.apache.log4j.Level/INFO
                            :off org.apache.log4j.Level/OFF
                            :trace org.apache.log4j.Level/TRACE
                            :trace-int org.apache.log4j.Level/TRACE_INT
                            :warn org.apache.log4j.Level/WARN))))))

(defn atom-logger []
  (let [a (atom "")
	l (fn [e & args] (swap! a (fn [x] (str e))))]
    [a l]))

(defn atom-counter []
  (let [s (atom {})
	update (fn [e f args]
		 (swap! s (fn [old] (update-in
				     old [(str e)]
				     (fn [x] (if x (inc x) 1))))))]
    [s update]))

;;TODO: we should probably smartly filter the stack traces.
(defn print-all [& keys]
  (fn [e f args]
    (let [ks (if (not (empty? keys))
	       keys
	       [:ex :stack :fn :args])]
      (pr-str
       (into {}
	     (map (fn [k]
		    [k
		     (condp = k
			     :ex (str e)
			     :stack
			     (map str (.getStackTrace e))
			     :fn (let [m (meta f)]
				   {:ns (str (:ns m))
				    :name (str (:name m))})
			     :args (map pr-str args))])
		  ks))))))

;;TODO: can test with a log appender fake.
;;http://www.mail-archive.com/log4j-user@logging.apache.org/msg08646.html
(defn logger [& [level keys]]
  (fn [e f args]
    (let [l (or level :debug)
	  m ((apply print-all keys) e f args)]
      (log/log l (with-out-str (pprint m))))))

(defn with-log
  ([f]
     (with-ex (logger) f))
  ([level f]
     (with-ex (logger level) f))
  ([level keys f]
     (with-ex (logger level keys) f)))

(defn with-trace
  ([f]
     (fn [& args]
       (log/trace (format "%s %s" f (prn-str args)))
       (apply f args))))

;; I believe this produces double output in logs as it adds a ConsoleAppender on the root logger.
;;(org.apache.log4j.BasicConfigurator/configure)