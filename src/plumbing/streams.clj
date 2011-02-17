(ns plumbing.streams)

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