(ns functional-data-structures.set)

(defprotocol Set
  (is-empty? [s])
  (is-member? [s value])
  (insert [s value]))

(defprotocol SmartSet
  (smart-insert [ss value])
  (smart-member? [ss value]))

(defprotocol Map
  (bind [m k v])
  (lookup [m k]))