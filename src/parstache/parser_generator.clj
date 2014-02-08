(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(declare add-to-tree)

(defn new-last-element [v new-element]
  (assoc-in v [(dec (count v))] new-element))

(defn add-immediate-children [intermediate-parse-tree legally-addable-children]
  (map
    (fn [to-add] (update-in
                   intermediate-parse-tree
                   [:children]
                   #(conj % to-add)))
    (legally-addable-children intermediate-parse-tree)))

(defn possible-new-subtrees [intermediate-parse-tree legally-addable-children]
  (let [last-child (last (:children intermediate-parse-tree))]
    (if last-child
      (add-to-tree last-child legally-addable-children)
      [])))

(defn update-last-child [intermediate-parse-tree new-child]
  (assoc-in
    intermediate-parse-tree
    [:children (dec (count (:children intermediate-parse-tree)))]
    new-child))

(defn with-altered-subtree [intermediate-parse-tree legally-addable-children]
  (map #(update-last-child intermediate-parse-tree %)
       (possible-new-subtrees intermediate-parse-tree legally-addable-children)))

(defn add-to-tree [intermediate-parse-tree legally-addable-children]
  (let [with-immediate-adds
        (add-immediate-children intermediate-parse-tree legally-addable-children)
        with-altered-subtree
        (with-altered-subtree intermediate-parse-tree legally-addable-children)]
    (concat with-altered-subtree with-immediate-adds)))


;(defn generate-parser [rules]
;  (fn [to-parse]
;    {:rule "REPEAT_A" :contents ["a" "a" "a"]}))

;closeable?
;if nil, true
;else,
;last child is closeable, and own constraints resolved
;
;add a rule, or add a token
;
;jkjk
