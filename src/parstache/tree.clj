(ns parstache.tree)

(defn find-node [predicate get-children tree]
  (if (predicate tree)
    tree
    (some
      #(find-node predicate get-children %)
      (get-children tree))))

(defn find-all [predicate get-children tree]
  (let [new-finds (if (predicate tree)
                    #{tree}
                    #{})]
    (reduce
      clojure.set/union
      new-finds
      (map #(find-all predicate get-children %) (get-children tree)))))
