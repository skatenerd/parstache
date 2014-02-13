(ns parstache.parser-generator
  (:require [parstache.tree :refer :all]))

(defprotocol Rule
  (r-closeable? [this node])
  (r-addable-children [this node all-rules remaining-program]))

(declare add-to-tree closeable? r-build-empty-node)

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

(defn build-empty-node [rule-name rules]
  (assoc (get rules rule-name) :children [] :name rule-name))

(defn addable-children [remaining-program rules node]
  (if (map? node)
    (let [{:keys [rule children]} node
          last-child-closeable? (if (map? (last children))
                                  (r-closeable? (:rule (last children)) (last children))
                                  true)]
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
    {:remaining-program program :tree (r-build-empty-node :root rules)}))

(defrecord Juxtaposition [name required-children]
  Rule
  (r-closeable? [this node]
    (let [child-names (map :name (:children node))]
      (= child-names required-children)))
  (r-addable-children [this node all-rules _]
    (let [has (:children node)]
      (if (= (count required-children) (count has))
        []
        [(r-build-empty-node (nth required-children (count has)) all-rules)]))))

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
    [(r-build-empty-node (first repeated-rule-name) all-rules)]))

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

(defn r-build-empty-node [rule-name all-rules]
  {:children []
   :rule (build-rule-with-name rule-name all-rules)})
