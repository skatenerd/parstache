(ns parstache.parser-generator.nodes
  (:require
    [parstache.vectors :refer [update-last-element]]))

(declare build-empty-node)

(defprotocol Node
  (closeable? [this])
  (addable-children [this all-rules remaining-program])
  (atoms [this]))

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

(defrecord Literal [contents children]
  Node
  (closeable? [this]
    true)
  (addable-children [this _ _]
    [])
  (atoms [this] [contents]))

(defrecord Juxtaposition [name children required-children]
  Node
  (closeable? [this]
    (let [child-names (map :name children)
          local-answer (= child-names required-children)]
      (and local-answer (closeable? (last children)))))
  (addable-children [this all-rules _]
    (let [last-child-closeable (or (empty? children) (closeable? (last children)))
          children-satisfied (= (count required-children) (count children))]
      (if (and last-child-closeable (not children-satisfied))
        [(build-empty-node (nth required-children (count children)) all-rules)]
        [])))
  (atoms [this] []))

(defrecord SingleCharacter [name children possible-characters]
  Node
  (closeable? [this]
    (not (empty? children)))
  (addable-children [this all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (if (empty? children)
        (map #(Literal. % []) (filter #(= % first-program-character) possible-characters))
        [])))
  (atoms [this] []))

(defrecord CharacterExclusion [name children unpossible-characters]
  Node
  (closeable? [this]
    (not (empty? children)))
  (addable-children [this all-rules remaining-program]
    (let [hates (set unpossible-characters)
          first-program-character (str (first remaining-program))]
    (if (empty? children)
      (if (contains? hates first-program-character)
        []
        [(Literal. first-program-character [])])
      []
      )))
  (atoms [this] []))

(defrecord Repetition [name children repeated-rule-name]
  Node
  (closeable? [this]
    true)
  (addable-children [this all-rules remaining-program]
    (if (or (empty? children) (closeable? (last children)))
      [(build-empty-node repeated-rule-name all-rules)]
      []))
  (atoms [this] []))

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
    (constructor (assoc rule-definition :children [] :name rule-name))))
