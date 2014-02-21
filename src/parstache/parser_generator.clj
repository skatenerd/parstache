(ns parstache.parser-generator
  (:require [clojure.walk :refer [postwalk]]
            [parstache.tree :refer :all]
            [parstache.parser-generator.nodes :refer :all]))

(declare add-to-tree)

(defn add-immediate-children [node remaining-program rules]
  (map
    #(add-child node %)
    (addable-children node rules remaining-program )))

(defn possible-new-subtrees [node remaining-program rules]
  (let [last-child (last (:children node))]
    (if last-child
      (add-to-tree last-child remaining-program rules)
      [])))

(defn with-altered-subtree [node remaining-program rules]
  (let [new-subtrees (possible-new-subtrees node remaining-program rules)]
    (map #(update-last-child node %)
         new-subtrees)))

(defn add-to-tree [node remaining-program rules]
  (let [with-immediate-adds
        (add-immediate-children node remaining-program rules)
        with-altered-subtree
        (with-altered-subtree node remaining-program rules)]
    (concat (reverse with-altered-subtree) with-immediate-adds)))

(defn string-leaves [tree]
  (apply str (mapcat :atoms (tree-seq map? :children tree))))

(defn get-parse-tree [rules program]
  (find-node
    (fn [state]
      (empty? (:remaining-program state)));predicate
    (fn [state]
      (let [reachable-trees (add-to-tree (:tree state) (:remaining-program state) rules)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str (drop (count (string-leaves reachable)) program))})
             reachable-trees)))
    {:remaining-program program :tree (build-empty-node :root rules)}))

(defn- decompose [composition]
  (mapv
    (fn [character]
      {:possible-characters [(str character)] :type :character})
    composition))

(defn build-one-or-more [node]
  (let [repeated-rule-name (:repeated-rule-name node)]
    {:type :juxtaposition :required-children [repeated-rule-name {:type :repetition :repeated-rule-name repeated-rule-name}]}))

(defn compile-rules [compilation-candidate]
  (postwalk
    (fn [node]
      (case (:type node)
        :word
        {:required-children (decompose (:allowed node)) :type :juxtaposition}
        :one-or-more
        (build-one-or-more node)
        node))
    compilation-candidate))
