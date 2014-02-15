(ns parstache.parser-generator.node-types)

(declare build-empty-node)
;
;(defn wat-closeable? [local-answer children]
;  (and local-answer (recur (last children))))

(defprotocol Rule
  (r-closeable? [this])
  (r-addable-children [this all-rules remaining-program]))

(defprotocol Leaf
  (leaf-stuff [this]))

(extend-type java.lang.Character
  Rule
  (r-closeable? [this]
    true)
  (r-addable-children [this _ _]
    [])
  Leaf
  (leaf-stuff [this] (str this)))

(defrecord Literal [actual-children]
  Rule
  (r-closeable? [this]
    true)
  (r-addable-children [this _ _]
    []))

(defrecord Juxtaposition [name actual-children required-children]
  Rule
  (r-closeable? [this]
    (let [child-names (map :name actual-children)
          local-answer (= child-names required-children)]
      (and local-answer (r-closeable? (last actual-children)))))
  (r-addable-children [this all-rules _]
    (let [has actual-children]
      (if (= (count required-children) (count has))
        []
        [(build-empty-node (nth required-children (count has)) all-rules)]))))

(defrecord SingleCharacter [name actual-children possible-characters]
  Rule
  (r-closeable? [this]
    (not (empty? actual-children)))
  (r-addable-children [this all-rules remaining-program]
    (let [first-program-character (str (first remaining-program))]
      (if (empty? actual-children)
        (map #(Literal. %) (filter #(= % first-program-character) possible-characters))
        []))))

(defrecord CharacterExclusion [name actual-children unpossible-characters]
  Rule
  (r-closeable? [this]
    (not (empty? actual-children)))
  (r-addable-children [this all-rules remaining-program]
    (let [hates (set unpossible-characters)
          first-program-character (str (first remaining-program))]
    (if (empty? actual-children)
      (if (contains? hates first-program-character)
        []
        [(Literal. first-program-character)])
      []
      ))))

(defrecord Repetition [name actual-children repeated-rule-name]
  Rule
  (r-closeable? [this]
    true)
  (r-addable-children [this all-rules remaining-program]
    [(build-empty-node repeated-rule-name all-rules)]))



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
