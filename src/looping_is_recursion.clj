(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base1 exp1]
                 (if (zero? exp1) acc (recur (* acc base1) base1 (dec exp1))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2) (not (== (first seq1) (first seq2)))) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         a-seq a-seq]
    (cond 
     (empty? a-seq) nil
     (pred (first a-seq)) n
     :else (recur (inc n) (rest a-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         length 0
         a-seq a-seq]
    (cond 
      (and (empty? a-seq) (zero? length)) 0
      (empty? a-seq) (/ acc length)
      :else (recur (+ acc (first a-seq)) (inc length) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem] (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [acc #{}
           a-seq a-seq]
      (cond
        (empty? a-seq) acc
        :else (recur (toggle acc (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (loop [prev 0
         curr 1
         n n]
    (cond
      (zero? n) prev
      :else (recur curr (+ prev curr) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         a-seq a-seq]
    (cond 
      (or (empty? a-seq) (some (partial = (first a-seq)) acc)) acc
      :else (recur (conj acc (first a-seq)) (rest a-seq)))))