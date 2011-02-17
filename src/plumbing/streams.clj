(ns plumbing.streams
  (:import java.util.concurrent.atomic.AtomicInteger))

(defn lazy-seq-of-seqs
  ([head-seq tail-seqs]
     (let [head-of-head (first head-seq)]
       (if head-of-head
	 [head-of-head
	  #(lazy-seq-of-seqs (rest head-seq) tail-seqs)]
	 (let [new-head (first tail-seqs)]
	   (if new-head
	     [(first new-head)
	      #(lazy-seq-of-seqs (rest new-head) (rest tail-seqs))]
	     nil)))))
  ([seq-of-seqs]
     (lazy-seq-of-seqs (first seq-of-seqs) (rest seq-of-seqs))))

(defn flat-iter [seq-of-seqs]
  (let [a (atom (lazy-seq-of-seqs seq-of-seqs))]
    (reify java.util.Iterator
	   (hasNext [this]
		    (not (nil? @a)))
	   (next [this]
		 (if @a
		   (let [v (first @a)
			 f (second @a)]
		     (swap! a (fn [old] (f)))		  
		     v)
		   (throw (java.util.NoSuchElementException.))))
	   (remove [this] (throw (java.lang.UnsupportedOperationException.))))))

(defn decrement [cnt len]
  (let [cur-cnt (.get cnt)]
    (.compareAndSet cnt
		    cur-cnt
		    (if (= -1 cur-cnt)
		      -1
		      (- cur-cnt len)))))

(defn test-stream [^bytes data & [num-repeats eof]]
  (let [cnt (AtomicInteger. (if num-repeats
			      (* (alength data)
				 num-repeats)
			      -1))
	pos (AtomicInteger. 0)]
    (proxy [java.io.InputStream] []
      (available []
		 (if eof
		   (+ (.get cnt) 1)
		   Integer/MAX_VALUE))
      (close [] nil)
      (mark [^int read-limit] nil)
      (markSupported [] false)
      (read ([]
	       (let [b (byte-array 1)]
		 (.read this b)
		 (aget b 0)))
	    ([^bytes b]
	       (.read this b 0 (alength b)))
	    ([^bytes b off len]
	       (let [idx (.get pos)
		     copy-len (min (- (alength b) off)
				   (- (alength data) idx))
		     padding-len (- len copy-len)
		     padding-off (+ off copy-len)]
		 (System/arraycopy data idx b off copy-len)
		 (when (> padding-len 0)
		   (System/arraycopy (byte-array padding-len eof)
				     0 b padding-off padding-len))
		 (decrement cnt copy-len)
		 (.compareAndSet pos
				 idx
				 (mod (+ idx copy-len)
				      (alength data)))
		 copy-len)))
      (reset [] nil)
      (skip [n] 0))))