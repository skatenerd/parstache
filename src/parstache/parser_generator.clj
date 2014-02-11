(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(declare add-to-tree closeable?)

(defn update-last-element [v new-element]
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
  (update-in
    intermediate-parse-tree
    [:children]
    #(update-last-element % new-child)))

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
  (assoc (get rules rule-name) :children [] :name rule-name))

(defn addable-to-juxtaposition [rules rule node]
  (let [wants (get rule :children)
        has (get node :children)]
    (if (= (count wants) (count has))
      []
      [(build-empty-node (nth wants (count has)) rules)])))

(defn addable-to-character [remaining-program rules rule node]
  (let [wants (get rule :children)
        first-program-character (first remaining-program)]
    (if (empty? (:children node))
      (filter #(= (first %) first-program-character) wants)
      [])))

(defn addable-to-exclusion [remaining-program rules rule node]
  (let [hates (set (get rule :children))
        first-program-character (str (first remaining-program))]
    (if (empty? (:children node))
      (if (contains? hates first-program-character)
        []
        [first-program-character]))))

(defn addable-to-repetition [rules rule node]
  (let [wants (get rule :children)]
    [(build-empty-node (first wants) rules)]))

(defn addable-children [remaining-program rules node]
  (if (map? node)
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
          (addable-to-repetition rules rule node)
          :exclusion
          (addable-to-exclusion remaining-program rules rule node))
        []))
    []))

(defn juxtaposition-closeable? [rule node]
  (let [child-names (map :name (:children node))]
    (= child-names (:children rule))))

(defn character-closeable? [rule node]
  (not (empty? (:children node))))

(defn exclusion-closeable? [rule node]
  (not (empty? (:children node))))

(defn closeable? [rules node]
  (if (map? node)
    (let [rule (get rules (:name node))
          local-answer (case (:type rule)
                         :juxtaposition
                         (juxtaposition-closeable? rule node)
                         :character
                         (character-closeable? rule node)
                         :exclusion
                         (exclusion-closeable? rule node)
                         :repetition
                         true)]
      (and local-answer (recur rules (last (:children node)))))
    true))

(defn string-leaves [tree]
  (filter string?  (tree-seq map? :children tree)))

(defn get-parse-trees [rules program]
  (find-all
    (fn [state]
      (empty? (:remaining-program state)));predicate
    (fn [state]
      (let [what-to-add (fn [tree] (addable-children (:remaining-program state) rules tree))
            reachable-trees (add-to-tree (:tree state) what-to-add)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str (drop (count (string-leaves reachable)) program))})
             reachable-trees)))
    {:remaining-program program :tree (build-empty-node :root rules)}))
