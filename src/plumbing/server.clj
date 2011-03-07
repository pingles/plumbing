(ns plumbing.server
  (:require [clj-json [core :as json]]
            [clojure.contrib.logging :as log])
  (:use	[clojure.contrib.def :only [defvar]]
        [clojure.string :only [lower-case]]
        [plumbing.serialize]
        [plumbing.core :only [silent]])
  (:import clojure.lang.RT
           (java.net InetAddress Socket
                     ServerSocket SocketException)
           (java.nio ByteBuffer)
           (java.util Arrays)
           (java.util.concurrent Executors)
           (java.io InputStream OutputStream
                    BufferedReader InputStreamReader
                    PrintWriter)))

(defn server [f reader writer
	      ^InputStream in ^OutputStream out]
  (->> in
       reader
       f
       (writer out))
  (.flush out))

(defn start [f port ^String bind-addr
             & {:keys [backlog num-threads]
                :or {backlog 128
                     num-threads 200}}]
  (let [ss (ServerSocket. port backlog (InetAddress/getByName bind-addr))
        pool (Executors/newFixedThreadPool num-threads)]
    (future (loop []
              (if-not (.isClosed ss)
                (do (try
                      (let [s (.accept ss)]
                        (.submit pool (cast Runnable #(f s))))
                      (catch SocketException e
                        (.printStackTrace e)))
                    (recur))
                (.shutdownNow pool))))
    ss))

(defn stop [^ServerSocket ss]
  (.close ss))

(defn client [^String host ^Integer port reader writer msg]
  (let [client (Socket. (InetAddress/getByName host) port)
        out (.getOutputStream client)
        in (.getInputStream client)]
    (writer out msg)
    (let [r (reader in)]
      (.close client)
      r)))