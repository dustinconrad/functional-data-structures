(ns functional-data-structures.stream)

(defn ++ [[x & s :as xs] t]
  (if (empty? xs) 
    t
    (lazy-seq (cons x (lazy-seq (++ s t))))
    ))
