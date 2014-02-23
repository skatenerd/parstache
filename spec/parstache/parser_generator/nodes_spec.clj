(ns parstache.parser-generator.nodes-spec
  (:require
    [parstache.parser-generator.nodes :refer :all]
    [speclj.core :refer :all]))

(context "juxtaposition nodes"
  (it "lets you add the first child to a juxtaposition"
    (let [grammar {:root {:type :juxtaposition :required-children [:first :middle]}
                   :first {:type :character :possible-characters ["a"]}
                   :middle {:type :character :possible-characters ["z"]}}
          node (build-empty-node :root grammar)]
      (should=
        [(build-empty-node :first grammar)]
        (addable-children
          node
          grammar
          "remaining program"))))

  (it "lets you add the second child to a juxtaposition"
    (let [grammar {:root {:type :juxtaposition :required-children [:first :middle]}
                   :first {:type :character :possible-characters ["a"]}
                   :middle {:type :character :possible-characters ["z"]}}
          first-child-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                  :name :first})
          node (map->Juxtaposition {:required-children [:first :middle]
                                    :children [first-child-node]
                                    :name :root})]
      (should=
        (build-empty-node :middle grammar)
        (first (addable-children
                 node
                 grammar
                 "remaining program")))))

  (it "does not let you add a child to juxtaposition when first child is not closeable"
    (let [grammar {:root {:type :juxtaposition :required-children [:first :middle]}
                   :first {:type :character :possible-characters ["a"]}
                   :middle {:type :character :possible-characters ["z"]}}
          first-child-node (map->SingleCharacter {:children []
                                                  :name :first})
          node (map->Juxtaposition {:required-children [:first :middle]
                                    :children [first-child-node]
                                    :name :root})]
      (should
        (empty? (addable-children
                  node
                  grammar
                  "remaining program"))))))


(context "character nodes"
  (it "lets you add to an empty single-character node, when the remaining program starts with right character"
    (let [grammar {:root {:type :character :possible-characters ["a" "b" "c"]}}
          node (build-empty-node :root grammar)]
      (should=
        [(->Literal ["a"] [])]
        (addable-children
          node
          grammar
          "abcdefg"))
      (should=
        [(->Literal ["b"] [])]
        (addable-children
          node
          grammar
          "bcdefg"))
      (should=
        []
        (addable-children
          node
          grammar
          "zzzzzzzzzzz"))
      (let [full-node (assoc-in node [:children] [(->Literal ["a"] [])])]
        (should=
          []
          (addable-children
            full-node
            grammar
            "aaaaaaaa")))))

  (it "lets you add to a character-exclusion node, when remainin program starts with right character"
      (let [grammar {:root {:type :exclusion :unpossible-characters ["a" "b" "c"]}}
            node (build-empty-node :root grammar)]
        (should=
          [(->Literal ["z"] [])]
          (addable-children
            node
            grammar
            "zzzzzzz"))
        (should=
          []
          (addable-children
            node
            grammar
            "aaaaaaaa")))))

(context "repetition nodes"
  (it "lets you add to a repetition rule when its children are closeable"
    (let [grammar {:root {:type :repetition :repeated-rule :char-rule}
                 :char-rule {:type :character :possible-characters ["Z"]}}
          node (build-empty-node :root grammar)]
      (should=
        parstache.parser_generator.nodes.SingleCharacter
        (type
          (first
            (addable-children
              node
              grammar
              "ZZZZZZZZZZZ"))))))

  (it "does not let you add to a repetition whose last child is unclosed"
    (let [grammar {:root {:type :repetition :repeated-rule :char-rule}
                 :char-rule {:type :character :possible-characters ["Z"]}}
          character-node (build-empty-node :char-rule grammar)
          node (map->Repetition {:children [character-node] :name :root})]
      (should=
        []
        (addable-children
          node
          grammar
          "ZZZZZZZZZZZ")))))

(context "or-nodes"
  (it "lets you have both options, when children are empty"
    (let [grammar {:root {:type :or :allowed-grammar [:a-char :b-char]}
                 :a-char {:type :character :possible-characters ["a"]}
                 :b-char {:type :character :possible-characters ["b"]}}
          node (build-empty-node :root grammar)]
      (should=
        [:a-char :b-char]
        (map
          :name
          (addable-children
            node
            grammar
            "ZZZZZZZZZZZ")))))

  (it "doesnt let you add anything when it has a child"
    (let [grammar {:root {:type :or :allowed-grammar [:a-char]}
                 :a-char {:type :character :possible-characters ["a"]}}
          character-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                :name :a-char})
          node (build-empty-node :root grammar)
          node (update-in node [:children] #(conj % character-node))]
      (should=
        []
        (addable-children
          node
          grammar
          "aaaaaaaaaaaaaa"))))

  (it "is not closeable unless it has closed children"
    (let [grammar {:root {:type :or :allowed-grammar [:a-char]}
                 :a-char {:type :character :possible-characters ["a"]}}
          character-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                :name :a-char})
          empty-node (build-empty-node :root grammar)
          full-node (update-in empty-node [:children] #(conj % character-node))]
      (should-not (closeable? empty-node))
      (should (closeable? full-node)))))
