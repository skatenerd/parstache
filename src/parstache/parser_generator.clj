(ns parstache.parser-generator
  (:require [clojure.walk :refer [postwalk]]
            clojure.pprint
            [parstache.tree :refer :all]
            [parstache.parser-generator.nodes :refer :all]))

(declare add-to-tree)

(defn add-immediate-children [node remaining-program grammar]
  (map
    #(add-child node %)
    (addable-children node grammar remaining-program )))

(defn possible-new-subtrees [node remaining-program grammar]
  (let [last-child (last (:children node))]
    (if last-child
      (add-to-tree last-child remaining-program grammar)
      [])))

(defn with-altered-subtree [node remaining-program grammar]
  (let [new-subtrees (possible-new-subtrees node remaining-program grammar)]
    (map #(update-last-child node %)
         new-subtrees)))

(defn add-to-tree [node remaining-program grammar]
  (let [with-immediate-adds
        (add-immediate-children node remaining-program grammar)
        with-altered-subtree
        (with-altered-subtree node remaining-program grammar)]
    (concat (reverse with-altered-subtree) with-immediate-adds)))

(defn string-leaves [tree]
  (apply str (mapcat :atoms (tree-seq map? :children tree))))

(defn node-count [tree]
  (count (tree-seq map? :children tree)))

(defn square [x]
  (* x x))

(defn get-parse-tree [grammar program]
  (best-first
    {:remaining-program program :tree (build-empty-node :root grammar)}
    ;not an admissible heuristic :-)
    #(square (count (:remaining-program %)))
    #(node-count (:tree %))
    (fn [state]
      (empty? (:remaining-program state)))
    (fn [state]
      (let [reachable-trees (add-to-tree (:tree state) (:remaining-program state) grammar)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str
                                          (drop (count (string-leaves reachable))
                                                program))})
             reachable-trees)))))

(defn- compile-word-node [node]
  (let [children (mapv
                   (fn [character]
                     {:possible-characters [(str character)] :type :character})
                   (:allowed node))]
 {:required-children children :type :juxtaposition}))

(defn build-one-or-more [node]
  (let [repeated-rule (:repeated-rule node)]
    {:type :juxtaposition :required-children [repeated-rule {:type :repetition :repeated-rule repeated-rule}]}))

(defn compile-grammar [compilation-candidate]
  (postwalk
    (fn [node]
      (case (:type node)
        :word
        (compile-word-node node)
        :one-or-more
        (build-one-or-more node)
        node))
    compilation-candidate))
