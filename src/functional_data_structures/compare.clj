(ns functional-data-structures.compare)

(defn lt? [x y]
  (neg? (compare x y)))

(defn gt? [x y]
  (pos? (compare x y)))

(defn lte? [x y]
  (not (gt? x y)))

(defn gte? [x y]
  (not (lt? x y)))

(defn eq? [x y]
  (zero? (compare x y)))