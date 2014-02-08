(ns parstache.tree)

(defn find-node [predicate get-children tree]
  (if (predicate tree)
    tree
    (some
      #(find-node predicate get-children %)
      (get-children tree))))
