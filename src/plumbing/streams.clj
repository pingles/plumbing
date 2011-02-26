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

(defn wrap-copy [from from-idx len to to-idx]
  (let [remain (min len
		    (- (alength from) from-idx))
	next-len (- len remain)]
    (System/arraycopy from from-idx to
		      to-idx remain)
    (when (> next-len 0)
      (recur from
	     (mod
	      (+ from-idx remain)
	      (alength from))
	     next-len
	     to
	     (+ to-idx remain)))))

(defn test-stream [^bytes data & [num-repeats eof]]
  (let [cnt (AtomicInteger. (if num-repeats
			      (* (alength data)
				 num-repeats)
			      -1))
	pos (AtomicInteger. 0)
	eof-pos (AtomicInteger. 0)
	eof-cnt (AtomicInteger. (if eof
				  (alength eof)
				  -1))]
    (proxy [java.io.InputStream] []
      (available []
		 (if eof
		   (+ (.get cnt) (.get eof-cnt))
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
		     remaining (.get cnt)
		     eof-idx (.get eof-pos)
		     eof-remaining (.get eof-cnt)
		     copy-len (max 0
				   (min remaining
					(- (alength b) off)
					(- (alength data) idx)))
		     padding-len (- len copy-len)
		     eof-len (if (> padding-len 0)
			       (min eof-remaining
				    padding-len)
			       0)]
		 (when (> copy-len 0)
		   (System/arraycopy data idx b off copy-len)
		   (decrement cnt copy-len)
		   (.compareAndSet pos
				   idx
				   (mod (+ idx copy-len)
					(alength data))))
		 (when (> eof-len 0)
		   (System/arraycopy
		    eof eof-idx b (+ off copy-len) eof-len)
		   (.compareAndSet eof-pos
				   eof-idx
				   (mod (+ eof-idx eof-len)
					(alength eof)))
		   (decrement eof-cnt eof-len))
		 copy-len)))
      (reset [] nil)
      (skip [n] 0))))