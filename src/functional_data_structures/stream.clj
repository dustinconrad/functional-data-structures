(ns functional-data-structures.stream
  (:refer-clojure :exclude [take drop reverse]))

(defn ++ [[x & s :as xs] t]
  (if (empty? xs) 
    t
    (lazy-seq (cons x (lazy-seq (++ s t))))
    ))

(defn take [n [x & s :as xs]]
  (cond
    (zero? n) (lazy-seq nil)
    (nil? xs) (lazy-seq nil)
    :else (lazy-seq (cons x (lazy-seq (take (dec n) s))))))

(defn drop [n [x & s :as xs]]
  (cond
    (zero? n) (lazy-seq xs)
    (nil? xs) (lazy-seq nil)
    :else (lazy-seq (drop (dec n) s))))

(defn reverse [s]
  (letfn [(reverse-prime [[x & s :as xs] r]
            (if (empty? xs)
              r
              (lazy-seq (reverse-prime s (lazy-seq (cons x r))))))]
    (reverse-prime s nil)))