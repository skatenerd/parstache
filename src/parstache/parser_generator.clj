(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(defn new-last-element [v new-element]
  (assoc-in v [(dec (count v))] new-element))

(defn add-to-tree [intermediate-parse-tree legally-addable-children]
  (let [can-add-here
        (legally-addable-children intermediate-parse-tree)
        with-immediate-adds
        (map
          (fn [to-add] (update-in
                         intermediate-parse-tree
                         [:children]
                         #(conj % to-add)))
          can-add-here)
        subtree-stuff
        (if (empty? (:children intermediate-parse-tree))
         []
         (add-to-tree (last (:children intermediate-parse-tree)) legally-addable-children))
        with-updated-last-child
        (map #(assoc-in
                intermediate-parse-tree
                [:children (dec (count (:children intermediate-parse-tree)))]
                %) subtree-stuff)
        ]
    (concat with-updated-last-child with-immediate-adds)))


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
