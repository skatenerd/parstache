(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]
            [parstache.parser-generator.node-types :refer :all]))

(declare add-to-tree closeable? addable-children build-rule-with-name)

(defn update-last-element [v new-element]
  (assoc-in v [(dec (count v))] new-element))

(defn add-immediate-children [node remaining-program rules]
  (map
    (fn [to-add] (update-in
                   node
                   [:actual-children]
                   #(conj % to-add)))
    (addable-children remaining-program rules node)))

(defn possible-new-subtrees [node remaining-program rules]
  (let [last-child (last (:actual-children node))]
    (if last-child
      (add-to-tree last-child remaining-program rules)
      [])))

(defn update-last-child [node new-child]
  (update-in
    node
    [:actual-children]
    #(update-last-element % new-child)))

(defn with-altered-subtree [node remaining-program rules]
  (let [new-subtrees (possible-new-subtrees node remaining-program rules)]
    (map #(update-last-child node %)
         new-subtrees)))

(defn add-to-tree [node remaining-program rules]
  (let [with-immediate-adds
        (add-immediate-children node remaining-program rules)
        with-altered-subtree
        (with-altered-subtree node remaining-program rules)]
    (concat with-altered-subtree with-immediate-adds)))

(defn addable-children [remaining-program rules node]
  (if (and (satisfies? Rule node) (closeable? (last (:actual-children node))))
    (r-addable-children node rules remaining-program)
    []))

(defn closeable? [node]
  (if (and node (satisfies? Rule node))
    (r-closeable? node)
    true))

(defn string-leaves [tree]
  (apply str (filter #(satisfies? Leaf %) (tree-seq map? :actual-children tree))))

(defn get-parse-tree [rules program]
  (find-node
    (fn [state]
      (empty? (:remaining-program state)));predicate
    (fn [state]
      ;(clojure.pprint/pprint state)
      (let [reachable-trees (add-to-tree (:tree state) (:remaining-program state) rules)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str (drop (count (string-leaves reachable)) program))})
             reachable-trees)))
    {:remaining-program program :tree (build-empty-node :root rules)}))
