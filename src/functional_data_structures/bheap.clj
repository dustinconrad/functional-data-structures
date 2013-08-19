(ns functional-data-structures.bheap)

(defn blah [{a :a b :b}]
  (do
    (println a)
    (println b)))

(blah {:b "c" :a "asdf"})