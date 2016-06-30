(ns looping-is-recursion)

(defn power [base exp]
 (let [helper (fn [acc n]
                (if (zero? n)
                  acc
                  (recur (* acc base) (dec n))))]
   (helper 1 exp)))


(defn last-element [a-seq]
 (let [helper (fn [last b-seq]
                (if (empty? b-seq)
                  last
                  (recur (first b-seq) (rest b-seq))))]
   (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                (cond
                  (and (empty? a-seq) (empty? b-seq)) true
                  (or (empty? a-seq) (empty? b-seq)) false
                  (not (= (first a-seq) (first b-seq))) false
                  :else (recur (rest a-seq) (rest b-seq))))]
   (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq1 a-seq]
    (cond
      (empty? seq1) nil
      (pred (first seq1)) index
      :else (recur (inc index) (rest seq1))))
  )

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0
         size 0
         seq1 a-seq]
     (if(empty? seq1)
       (/ sum size)
       (recur (+ sum (first seq1)) (inc size) (rest seq1))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (loop [result #{}
         in a-seq]
    (if (empty? in)
      result
      (recur (toggle result (first in)) (rest in)))))

(defn fast-fibo [n]
	   (cond
	     (== n 0) 0
	     (== n 1) 1
	     :else (loop [ind 2
                    result 1
                    prev-res 1]
              (if (== ind n)
                result
                (recur (inc ind) (+ result prev-res) result)))))

(defn cut-at-repetition [a-seq]
  (loop [result []
         in a-seq]
    (if (or (empty? in) (contains? (set result) (first in)))
      result
      (recur (conj result (first in)) (rest in)))))

