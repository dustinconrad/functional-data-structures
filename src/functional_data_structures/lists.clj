(ns functional-data-structures.lists)

(defn ++ [xs ys]
  (if (empty? xs)
    ys
    (cons (first xs) (++ (rest xs) ys))))

(defn update [lst i y]
  (cond
    (empty? lst) (throw (IndexOutOfBoundsException.))
    (zero? i) (cons y (rest lst))
    :else (cons (first lst) (update (rest lst) (dec i) y))))