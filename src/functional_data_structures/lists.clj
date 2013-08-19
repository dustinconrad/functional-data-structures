(ns functional-data-structures.lists)

(defn ++ [xs ys]
  (if (empty? xs)
    ys
    (cons (first xs) (++ (rest xs) ys))))

(defn update [[x & xs :as lst] i y]
  (cond
    (empty? lst) (throw (IndexOutOfBoundsException.))
    (zero? i) (cons y xs)
    :else (cons x (update xs (dec i) y))))

(defn suffixes [xs]
  (if (empty? xs)
    [xs]
    (cons xs (suffixes (rest xs)))))