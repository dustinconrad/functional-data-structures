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

(defn insertion-sort [coll k]
  (letfn [(insert [[x & s :as xs] y k]
            (do (println "insert" xs y k)
            (cond
              (empty? xs) (list y)
              (zero? k) nil
              (< y x) (cons y xs)
              :else (cons x (insert s y (dec k))))))
          (ins-sort [[t & u :as tu] k xs]
            (do
              (println "ins-sort" tu k xs)
            (cond
              (empty? tu) xs
              :else (ins-sort u k (insert xs t k)))))]
    (ins-sort coll k nil)))

(let [c (shuffle (range 10))]
  (do (println c)
    (insertion-sort c 4)))