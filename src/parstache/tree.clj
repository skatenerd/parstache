(ns parstache.tree
  (:require
    clojure.pprint))

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

(defn- new-frontier [frontier best-frontier-choice get-neighbors heuristic cost]
  (let [best-removed (disj frontier best-frontier-choice)
        to-add (map (fn [neighbor] {:node neighbor
                                    :cost (cost neighbor)
                                    :heuristic (heuristic neighbor)})
                    (get-neighbors (:node best-frontier-choice)))]
    (if (empty? to-add)
      best-removed
      (apply conj best-removed to-add))))

(defn best-choice [frontier]
  (let [answer (apply min-key #(+ (:cost %) (:heuristic %)) frontier)]
    answer))

(def empty-frontier #{})

(defn best-first-private [frontier heuristic cost stopping-criteria get-neighbors]
  (let [best-frontier-choice (best-choice frontier)]
    (if (stopping-criteria (:node best-frontier-choice))
      (:node best-frontier-choice)
      (recur
        (new-frontier frontier best-frontier-choice get-neighbors heuristic cost)
        heuristic
        cost
        stopping-criteria
        get-neighbors))))

(defn best-first [start-node heuristic cost stopping-criteria get-neighbors]
  (best-first-private
    (conj empty-frontier {:node start-node
                          :cost (cost start-node)
                          :heuristic (heuristic start-node)})
    heuristic
    cost
    stopping-criteria
    get-neighbors))
