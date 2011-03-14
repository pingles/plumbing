(ns plumbing.prototype)

(deftype PrototypeObject [method-map obj-data]

  ;; Associative on object data
  clojure.lang.Associative
  (assoc [this k v]
    (PrototypeObject. method-map (assoc obj-data k v)))
  (entryAt [this k]
    (find obj-data k))
  (containsKey [this k] (find obj-data k))

  ;; Keyword lookup on obj-data
  clojure.lang.ILookup
  (valAt [this k] (obj-data k))
  (valAt [this k not-found] (get obj-data k not-found))

  ;; Fn execution on methods
  clojure.lang.IFn
  (invoke [this method]
    ((method method-map) obj-data))
  (invoke [this method arg1]
     ((method method-map) obj-data arg1))
  (invoke [this method arg1 arg2]
     ((method method-map) obj-data arg1 arg2))
  (invoke [this method arg1 arg2 arg3]
     ((method method-map) obj-data arg1 arg2 arg3))
  (invoke [this method arg1 arg2 arg3 arg4]
     ((method method-map) obj-data arg1 arg2 arg3 arg4))
  (applyTo [this args]
      (let [[method & other-args] args]
	(apply (method method-map) obj-data other-args))))

(defn responds-to? [^PrototypeObject obj method]
  (method (.method-map obj)))

(defn get-methods [^PrototypeObject o]
  (.method-map o))

(defn add-methods [^PrototypeObject obj  method-map]
  (PrototypeObject.
     (merge (.method-map obj) method-map)
     (.obj-data obj)))

(defn mix [^PrototypeObject o1 ^PrototypeObject o2]
  (PrototypeObject.
    (merge (.method-map o1) (.method-map o2))
    (merge (.obj-data o1) (.obj-data o2))))

(defn empty-obj
  ([] (PrototypeObject. {} {})))













