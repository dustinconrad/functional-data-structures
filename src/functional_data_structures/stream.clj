(ns functional-data-structures.stream
  (:refer-clojure :exclude [take]))

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
