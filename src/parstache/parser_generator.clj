(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(defprotocol Rule
  (r-closeable? [this node])
  (r-addable-children [this node all-rules remaining-program]))

(declare add-to-tree closeable? build-empty-node addable-children)

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

(defn get-parse-tree [rules program]
  (find-node
    (fn [state]
      (empty? (:remaining-program state)));predicate
    (fn [state]
      (prn (:remaining-program state))
      (let [reachable-trees (add-to-tree (:tree state) (:remaining-program state) rules)]
        (map (fn [reachable]
               {:tree reachable
                :remaining-program (apply str (drop (count (string-leaves reachable)) program))})
             reachable-trees)))
    {:remaining-program program :tree (build-empty-node :root rules)}))

(defrecord Juxtaposition [name required-children]
  Rule
  (r-closeable? [this node]
    (let [child-names (map :name (map :rule (:children node)))]
      (= child-names required-children)))
  (r-addable-children [this node all-rules _]
    (let [has (:children node)]
      (if (= (count required-children) (count has))
        []
        [(build-empty-node (nth required-children (count has)) all-rules)]))))

(defrecord SingleCharacter [name possible-characters]
  Rule
  (r-closeable? [this node]
    (not (empty? (:children node))))
  (r-addable-children [this node all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (if (empty? (:children node))
        (filter #(= % first-program-character) possible-characters)
        []))))

(defrecord CharacterExclusion [name unpossible-characters]
  Rule
  (r-closeable? [this node]
    (not (empty? (:children node))))
  (r-addable-children [this node all-rules remaining-program]
    (let [hates (set unpossible-characters)
          first-program-character (str (first remaining-program))]
    (if (empty? (:children node))
      (if (contains? hates first-program-character)
        []
        [first-program-character])))))

(defrecord Repetition [name repeated-rule-name]
  Rule
  (r-closeable? [this node]
    true)
  (r-addable-children [this node all-rules remaining-program]
    [(build-empty-node (first repeated-rule-name) all-rules)]))

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

(defn build-empty-node [rule-name all-rules]
  {:children []
   :rule (build-rule-with-name rule-name all-rules)})
