(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(declare add-to-tree closeable?)

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

(defn rules-with-metadata [rules]
  (into {} (map (fn [[key val]] [key (assoc val :name key)]) rules)))

(defn build-empty-node [rule-name rules]
  (assoc (get rules rule-name) :children []))

(defn addable-to-juxtaposition [rules rule node]
  (let [wants (get rule :children)
        has (get node :children)
        next-up (nth wants (count has))]
    [(build-empty-node next-up rules)]))

(defn addable-to-character [remaining-program rules rule node]
  (let [wants (get rule :children)
        first-program-character (first remaining-program)]
    (filter #(= (first %) first-program-character) wants)))

(defn addable-to-repetition [rules rule node]
  (let [wants (get rule :children)]
    [(build-empty-node (first wants) rules)]))

(defn addable-children [remaining-program rules node]
  (let [rules (rules-with-metadata rules)
        rule (get rules (:name node))
        last-child-closeable? (closeable? rules (last (:children node)))]
    (if last-child-closeable?
      (case (:type rule)
        :juxtaposition
        (addable-to-juxtaposition rules rule node)
        :character
        (addable-to-character remaining-program rules rule node)
        :repetition
        (addable-to-repetition rules rule node))
      [])))

(defn juxtaposition-closeable? [rule node]
  (let [child-names (map :name (:children node))]
    (= child-names (:children rule))))

(defn character-closeable? [rule node]
  (not (empty? (:children node))))

(defn closeable? [rules node]
  (if (map? node)
    (let [rule (get rules (:name node))
          local-answer (case (:type rule)
                         :juxtaposition
                         (juxtaposition-closeable? rule node)
                         :character
                         (character-closeable? rule node)
                         :repetition
                         true)]
      (and local-answer (recur rules (last (:children node)))))
    true))
