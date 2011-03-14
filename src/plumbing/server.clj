(ns plumbing.server
  (:require [clj-json [core :as json]]
            [clojure.contrib.logging :as log])
  (:use	[clojure.contrib.def :only [defvar]]
	[clojure.string :only [lower-case]]
	[plumbing.serialize]
	[plumbing.core :only [silent]]
	[clojure.contrib.server-socket
	 :only [create-server
		close-server]])
  (:import clojure.lang.RT
	   (java.net InetAddress Socket)
	   (java.nio ByteBuffer)
	   (java.util Arrays)
	   (java.io InputStream OutputStream
		    BufferedReader InputStreamReader
		    PrintWriter)))

(defn server
  "execute fn f on messager obtained from reader on InputStream in
  and write f response to writer and finally to OutputStream out"
  [f reader writer ^InputStream in ^OutputStream out]	      
  (->> in
       reader
       f
       (writer out))
  (.flush out))

(defn start [f
             & {:keys [port backlog bind-addr]
                :or {port 4444
                     backlog 50
                     bind-addr (InetAddress/getByName "127.0.0.1")}}]
  (create-server port f backlog bind-addr))

(defn client [^String host ^Integer port reader writer msg]
  (let [client (Socket. (InetAddress/getByName host) port)
        out (.getOutputStream client)
        in (.getInputStream client)]
    (writer out msg)
    (let [r (reader in)]
      (.close client)
      r)))