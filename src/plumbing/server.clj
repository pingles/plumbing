(ns plumbing.server
  (:require [clj-json [core :as json]]
            [clojure.contrib.logging :as log])
  (:use	[clojure.contrib.def :only [defvar]]
	[clojure.string :only [lower-case]]
	[plumbing.serialize]
	[plumbing.core :only [silent]]
	[clojure.contrib.server-socket :only [create-server
                                              close-server]])
  (:import clojure.lang.RT
	   (java.net InetAddress Socket)
	   (java.nio ByteBuffer)
	   (java.util Arrays)
	   (java.io InputStream OutputStream
		    BufferedReader InputStreamReader
		    PrintWriter)))

(defn server [f reader writer
	      ^InputStream in ^OutputStream out]
  (->> in
       reader
       f
       (writer out))
  (.flush os))

(defn start [fun
             & {:keys [port backlog bind-addr]
                :or {port 4444
                     backlog 50
                     bind-addr (InetAddress/getByName "127.0.0.1")}}]
  (let [server (create-server port fun backlog bind-addr)]
    server))

(defn client [msg reader writer
	      ^InputStream in
	      ^OutputStream out]
  (writer out cmd)
  (reader in))

(defn client-socket [^String host ^Integer port f]
  (let [client (Socket. (InetAddress/getByName host) port)
        os (.getOutputStream client)
        ins (.getInputStream client)]
    (f ins os)))