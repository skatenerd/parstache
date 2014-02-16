(ns parstache.parser-generator.nodes
  (:require
    [parstache.vectors :refer [update-last-element]]))

(declare build-empty-node)

(defprotocol Node
  (closeable? [this])
  (addable-children [this all-rules remaining-program])
  (atoms [this])
  (branches [this]))

(defn update-last-child [node new-child]
  (update-in
    node
    [:actual-children]
    #(update-last-element % new-child)))

(defrecord Literal [actual-children]
  Node
  (closeable? [this]
    true)
  (addable-children [this _ _]
    [])
  (atoms [this] [actual-children])
  (branches [this] []))

(defrecord Juxtaposition [name actual-children required-children]
  Node
  (closeable? [this]
    (let [child-names (map :name actual-children)
          local-answer (= child-names required-children)]
      (and local-answer (closeable? (last actual-children)))))
  (addable-children [this all-rules _]
    (let [last-child-closeable (or (empty? actual-children) (closeable? (last actual-children)))
          children-satisfied (= (count required-children) (count actual-children))]
      (if (and last-child-closeable (not children-satisfied))
        [(build-empty-node (nth required-children (count actual-children)) all-rules)]
        [])))
  (atoms [this] [])
  (branches [this] actual-children))

(defrecord SingleCharacter [name actual-children possible-characters]
  Node
  (closeable? [this]
    (not (empty? actual-children)))
  (addable-children [this all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (if (empty? actual-children)
        (map #(Literal. %) (filter #(= % first-program-character) possible-characters))
        [])))
  (atoms [this] [])
  (branches [this] actual-children))

(defrecord CharacterExclusion [name actual-children unpossible-characters]
  Node
  (closeable? [this]
    (not (empty? actual-children)))
  (addable-children [this all-rules remaining-program]
    (let [hates (set unpossible-characters)
          first-program-character (str (first remaining-program))]
    (if (empty? actual-children)
      (if (contains? hates first-program-character)
        []
        [(Literal. first-program-character)])
      []
      )))
  (atoms [this] [])
  (branches [this] actual-children))

(defrecord Repetition [name actual-children repeated-rule-name]
  Node
  (closeable? [this]
    true)
  (addable-children [this all-rules remaining-program]
    (if (or (empty? actual-children) (closeable? (last actual-children)))
      [(build-empty-node repeated-rule-name all-rules)]
      []))
  (atoms [this] [])
  (branches [this] actual-children))



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
    (constructor (assoc rule-definition :actual-children [] :name rule-name))))
