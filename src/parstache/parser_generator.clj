(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]
            [parstache.parser-generator.node-types :refer :all]))

(declare add-to-tree closeable? addable-children build-rule-with-name)

(defn update-last-element [v new-element]
  (assoc-in v [(dec (count v))] new-element))

(defn add-immediate-children [intermediate-parse-tree remaining-program rules]
  (map
    (fn [to-add] (update-in
                   intermediate-parse-tree
                   [:children]
                   #(conj % to-add)))
    (addable-children remaining-program rules intermediate-parse-tree)))

(defn possible-new-subtrees [intermediate-parse-tree remaining-program rules]
  (let [last-child (last (:children intermediate-parse-tree))]
    (if last-child
      (add-to-tree last-child remaining-program rules)
      [])))

(defn update-last-child [intermediate-parse-tree new-child]
  (update-in
    intermediate-parse-tree
    [:children]
    #(update-last-element % new-child)))

(defn with-altered-subtree [intermediate-parse-tree remaining-program rules]
  (let [new-subtrees (possible-new-subtrees intermediate-parse-tree remaining-program rules)]
    (map #(update-last-child intermediate-parse-tree %)
         new-subtrees)))

(defn add-to-tree [intermediate-parse-tree remaining-program rules]
  (let [with-immediate-adds
        (add-immediate-children intermediate-parse-tree remaining-program rules)
        with-altered-subtree
        (with-altered-subtree intermediate-parse-tree remaining-program rules)]
    (concat with-altered-subtree with-immediate-adds)))

(defn addable-children [remaining-program rules node]
  (if (map? node)
    (let [{:keys [rule children]} node
          last-child-closeable? (closeable? rules (last children))]
      (if last-child-closeable?
        (r-addable-children rule node rules remaining-program)
        []))
    []))

(defn closeable? [rules node]
  (if (map? node)
    (let [{:keys [rule children]} node
          local-answer (r-closeable? rule node)]
      (and local-answer (recur rules (last (:children node)))))
    true))

(defn string-leaves [tree]
  (filter string?  (tree-seq map? :children tree)))

(defn recordify-rules [rules]
  (into {} (map (fn [[rule-name contents]] [rule-name (build-rule-with-name rule-name rules)]) rules)))

(defn get-parse-tree [rules program]
  (let [rules (recordify-rules rules)]
    (find-node
    (fn [state]
      (empty? (:remaining-program state)));predicate
    (fn [state]
      (let [reachable-trees (add-to-tree (:tree state) (:remaining-program state) rules)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str (drop (count (string-leaves reachable)) program))})
             reachable-trees)))
    {:remaining-program program :tree (build-empty-node :root rules)})))

(defn build-rule-with-name [rule-name all-rules]
  (let [{:keys [type children]} (get all-rules rule-name)
        constructor (case type
                      :juxtaposition
                      ->Juxtaposition
                      :character
                      ->SingleCharacter
                      :exclusion
                      ->CharacterExclusion
                      :repetition
                      ->Repetition)]
    (constructor rule-name children)))


