(ns parstache.parser-generator.nodes
  (:require
    [parstache.vectors :refer [update-last-element]]))

(declare build-empty-node)

(defprotocol Node
  (closeable? [this])
  (addable-children [this all-rules remaining-program]))

(defn update-last-child [node new-child]
  (update-in
    node
    [:children]
    #(update-last-element % new-child)))

(defn add-child [node new-child]
  (update-in
    node
    [:children]
    #(conj % new-child)))

(defrecord Literal [atoms children]
  Node
  (closeable? [this]
    true)
  (addable-children [this _ _]
    []))

(defn addable-if-children-closeable [children condition return]
  (let [last-child-closeable (or (empty? children) (closeable? (last children)))]
    (if (and last-child-closeable condition)
      (return)
      [])))

(defrecord Juxtaposition [name children required-children atoms]
  Node
  (closeable? [this]
    (let [child-names (map :name children)
          local-answer (= child-names required-children)]
      (and local-answer (closeable? (last children)))))
  (addable-children [this all-rules _]
    (let [children-satisfied (= (count required-children) (count children))
          build-new-node #(vector (build-empty-node (nth required-children (count children)) all-rules)) ]
      (addable-if-children-closeable
        children
        (not children-satisfied)
        build-new-node))))

(defrecord SingleCharacter [name children possible-characters atoms]
  Node
  (closeable? [this]
    (not (empty? children)))
  (addable-children [this all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (addable-if-children-closeable
        children
        (contains? (set possible-characters) first-program-character)
        #(vector (Literal. [first-program-character] []))))))

(defrecord CharacterExclusion [name children unpossible-characters atoms]
  Node
  (closeable? [this]
    (not (empty? children)))
  (addable-children [this all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (addable-if-children-closeable
        children
        (not (contains? (set unpossible-characters) first-program-character))
        #(vector (Literal. [first-program-character] []))))))

(defrecord Repetition [name children repeated-rule-name atoms]
  Node
  (closeable? [this]
    true)
  (addable-children [this all-rules remaining-program]
    (addable-if-children-closeable
      children
      true
      #(vector (build-empty-node repeated-rule-name all-rules)))))

(defn build-empty-node [rule-name all-rules]
  (let [rule-definition (get all-rules rule-name)
        constructor (case (:type rule-definition)
                      :juxtaposition
                      map->Juxtaposition
                      :character
                      map->SingleCharacter
                      :exclusion
                      map->CharacterExclusion
                      :repetition
                      map->Repetition)]
    (constructor (assoc rule-definition :children [] :name rule-name :atoms []))))
