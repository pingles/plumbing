(ns plumbing.classpath
  (:use clojure.contrib.java-utils
        [clojure.contrib.duck-streams :only [slurp*]]
	[clojure.contrib.classpath :only [classpath-directories]]
	[clojure.contrib.find-namespaces :only [find-namespaces-in-dir]]))

(defn read-from-classpath
  "Returns a String for the classpath resource at the given path."
  [path]
  (slurp*
   (.getResourceAsStream
    (.getClassLoader
     (class *ns*)) path)))

(defn path-to-resource [resource]
  (.getPath
   (.getResource
    (.getClassLoader (class *ns*))
    resource)))

(defn this-path [] (.getCanonicalPath (java.io.File. ".")))
(defn path-exists? [path] (.exists (java.io.File. path)))

(defn ns-path
  [ns-name]
  (let [dirs (classpath-directories)
        ns-dir (first (filter (fn [d] (contains? (set (find-namespaces-in-dir d))
                                                 (symbol ns-name))) 
                              dirs))]
    (if ns-dir
      (.getCanonicalFile ns-dir)
      nil)))

(defn parent-path
  "Given a ns name, return project root path as string."
  [nsname]
  (-> (ns-path nsname)
      .getParentFile
      .getCanonicalPath))