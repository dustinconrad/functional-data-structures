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

; exercise 2.1
(defn suffixes [[x & xs :as lst]]
  (if (empty? lst)
    [(list)]
    (cons lst (suffixes xs))))