(ns parstache.parser-generator-spec
  (:require
    [parstache.core :refer [mustache-specification]]
    [parstache.parser-generator :refer :all]
    [clojure.pprint]
    [parstache.parser-generator.nodes :refer :all]
    [speclj.core :refer :all]))

(context "juxtaposition nodes"
  (it "lets you add the first child to a juxtaposition"
      (let [rules {:root {:type :juxtaposition :required-children [:first :middle]}
                        :first {:type :character :possible-characters ["a"]}
                        :middle {:type :character :possible-characters ["z"]}}
            node (build-empty-node :root rules)]
        (should=
          [(build-empty-node :first rules)]
          (addable-children
            node
            rules
            "remaining program"))))

  (it "lets you add the second child to a juxtaposition"
      (let [rules {:root {:type :juxtaposition :required-children [:first :middle]}
                        :first {:type :character :possible-characters ["a"]}
                        :middle {:type :character :possible-characters ["z"]}}
            first-child-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                    :name :first})
            node (map->Juxtaposition {:required-children [:first :middle]
                                      :children [first-child-node]
                                      :name :root})]
        (should=
          (build-empty-node :middle rules)
          (first (addable-children
                   node
                   rules
                   "remaining program")))))

  (it "does not let you add a child to juxtaposition when first child is not closeable"
    (let [rules {:root {:type :juxtaposition :required-children [:first :middle]}
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
                  rules
                  "remaining program"))))))

(context "character rules"
  (it "lets you add to an empty single-character node, when the remaining program starts with right character"
    (let [rules {:root {:type :character :possible-characters ["a" "b" "c"]}}
          node (build-empty-node :root rules)]
      (should=
        [(->Literal ["a"] [])]
        (addable-children
          node
          rules
          "abcdefg"))
      (should=
        [(->Literal ["b"] [])]
        (addable-children
          node
          rules
          "bcdefg"))
      (should=
        []
        (addable-children
          node
          rules
          "zzzzzzzzzzz"))
      (let [full-node (assoc-in node [:children] [(->Literal ["a"] [])])]
        (should=
          []
          (addable-children
            full-node
            rules
            "aaaaaaaa")))))

  (it "lets you add to a character-exclusion node, when remainin program starts with right character"
      (let [rules {:root {:type :exclusion :unpossible-characters ["a" "b" "c"]}}
            node (build-empty-node :root rules)]
        (should=
          [(->Literal ["z"] [])]
          (addable-children
            node
            rules
            "zzzzzzz"))
        (should=
          []
          (addable-children
            node
            rules
            "aaaaaaaa")))))

(context "repetition rules"
  (it "lets you add to a repetition rule when its children are closeable"
    (let [rules {:root {:type :repetition :repeated-rule :char-rule}
                 :char-rule {:type :character :possible-characters ["Z"]}}
          node (build-empty-node :root rules)]
      (should=
        parstache.parser_generator.nodes.SingleCharacter
        (type
          (first
            (addable-children
              node
              rules
              "ZZZZZZZZZZZ"))))))

  (it "does not let you add to a repetition whose last child is unclosed"
    (let [rules {:root {:type :repetition :repeated-rule :char-rule}
                 :char-rule {:type :character :possible-characters ["Z"]}}
          character-node (build-empty-node :char-rule rules)
          node (map->Repetition {:children [character-node] :name :root})]
      (should=
        []
        (addable-children
          node
          rules
          "ZZZZZZZZZZZ")))))

(context "or-rules"
  (it "lets you have both options, when children are empty"
    (let [rules {:root {:type :or :allowed-rules [:a-char :b-char]}
                 :a-char {:type :character :possible-characters ["a"]}
                 :b-char {:type :character :possible-characters ["b"]}}
          node (build-empty-node :root rules)]
      (should=
        [:a-char :b-char]
        (map
          :name
          (addable-children
            node
            rules
            "ZZZZZZZZZZZ")))))

  (it "doesnt let you add anything when it has a child"
    (let [rules {:root {:type :or :allowed-rules [:a-char]}
                 :a-char {:type :character :possible-characters ["a"]}}
          character-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                :name :a-char})
          node (build-empty-node :root rules)
          node (update-in node [:children] #(conj % character-node))]
      (should=
        []
        (addable-children
          node
          rules
          "aaaaaaaaaaaaaa"))))

  (it "is not closeable unless it has closed children"
    (let [rules {:root {:type :or :allowed-rules [:a-char]}
                 :a-char {:type :character :possible-characters ["a"]}}
          character-node (map->SingleCharacter {:children [(->Literal "a" [])]
                                                :name :a-char})
          empty-node (build-empty-node :root rules)
          full-node (update-in empty-node [:children] #(conj % character-node))]
      (should-not (closeable? empty-node))
      (should (closeable? full-node)))))

(describe "integration"
  (it "...works?"
    (let [rules {:root {:type :juxtaposition :required-children [:a-char
                                                                 :b-char
                                                                 :repeat-root
                                                                 :a-char]}
                 :a-char {:type :character :possible-characters ["a"]}
                 :b-char {:type :character :possible-characters ["b"]}
                 :repeat-root {:type :repetition :repeated-rule :root}}
          program "ababaa"]
      (should-not (empty? (get-parse-tree rules program)))))

  (it "does harder lisp"
    (let [rules {:root {:type :juxtaposition :required-children [:open-paren
                                                                 :non-parens
                                                                 :repeat-root
                                                                 :non-parens
                                                                 :close-paren
                                                                 :non-parens]}
                 :open-paren {:type :character :possible-characters ["("]}
                 :close-paren {:type :character :possible-characters [")"]}
                 :repeat-root {:type :repetition :repeated-rule :root}
                 :non-paren {:type :exclusion :unpossible-characters ["(" ")"]}
                 :non-parens {:type :repetition :repeated-rule :non-paren}
                 }
          program "(+ 1 (* 3 4) (* 2 3))"]
      (should= program (string-leaves (:tree (get-parse-tree rules program))))))

  (it "does mustache"
    (let [rules (compile-rules mustache-specification)
          program "{{#sup}}bro {{hi}}  {{/sup}} hi {{wut}} here is {{>a_partial}}"]
      (should= program (string-leaves (:tree (get-parse-tree rules program))))))


  (it "supports nested rules"
    (let [rules {:root {:type :juxtaposition
                        :required-children [{:type :character
                                             :possible-characters ["a"]}
                                            {:type :character
                                             :possible-characters ["b"]}
                                            :repeat-root
                                            {:type :character
                                             :possible-characters ["a"]}]}

                 :repeat-root {:type :repetition :repeated-rule :root}}
          program "abababa"]
      (should-not (empty? (get-parse-tree rules program))))))


(context "rule compiling"
  (it "empty"
    (let [to-compile {}
          expected {}]
      (should= expected (compile-rules to-compile))))
  (it "word-rules"
    (let [to-compile {:root {:type :word :allowed "YO"}}
          expected {:root {:type :juxtaposition
                           :required-children [{:type :character :possible-characters ["Y"]}
                                               {:type :character :possible-characters ["O"]}]}}]

      (should= expected (compile-rules to-compile))))

  (it "one-or-more"
    (let [to-compile {:root {:type :one-or-more :repeated-rule :foo}
                      :foo {:type :character :dont :care}}
          expected {:root {:type :juxtaposition
                           :required-children [:foo
                                               {:type :repetition :repeated-rule :foo}]}
                    :foo {:type :character :dont :care}}]
      (should= expected (compile-rules to-compile)))))
