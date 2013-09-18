(ns functional-data-structures.set)

(defprotocol Set
  (is-empty? [this])
  (is-member? [this value])
  (insert [this value]))

(defprotocol SmartSet
  (smart-insert [this value])
  (smart-member? [this value]))