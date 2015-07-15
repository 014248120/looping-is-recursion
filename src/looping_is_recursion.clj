(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                  (if (zero? n)
                     acc
                     (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc b-seq]
                  (if (zero? (count b-seq))
                     acc
                     (recur (first b-seq) (rest b-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                  (cond (and (empty? a-seq) (empty? b-seq)) true
                        (or (empty? a-seq) (empty? b-seq)) false
                        (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
                        :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [b-seq a-seq
         n 0]
    (cond (empty? b-seq) nil
          (pred (first b-seq)) n
       :else (recur (rest b-seq) (inc n)))))

(defn avg [a-seq]
  (loop [b-seq a-seq
         maara 0
         summa 0]
    (cond (and (empty? b-seq) (zero? maara)) 0
          (empty? b-seq) (/ summa maara)
          :else (recur (rest b-seq) (inc maara) (+ summa (first b-seq))))))

(defn parity [a-seq]
  (let [toggle (fn [x-set elem]
                  (if (contains? x-set elem)
                  (disj x-set elem)
                  (conj x-set elem)))]
    (loop [b-seq a-seq
         a-set #{}]
    (if (empty? b-seq)
       a-set
       (recur (rest b-seq) (toggle a-set (first b-seq)))))))

(defn fast-fibo [n]
  (loop [laskuri 0
         f_k 0
         f_kplus1 1]
    (if (= laskuri n)
      f_k
      (recur (inc laskuri) f_kplus1 (+ f_k f_kplus1)))))

(defn cut-at-repetition [a-seq]
  (loop [x-set #{}
         x-seq []
         b-seq a-seq]
    (if (or (empty? b-seq) (contains? x-set (first b-seq)))
        x-seq
        (recur (conj x-set (first b-seq)) (conj x-seq (first b-seq)) (rest b-seq)))))

