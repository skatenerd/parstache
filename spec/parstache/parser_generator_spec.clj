(ns parstache.parser-generator-spec
  (:require
    [parstache.parser-generator :refer :all]
    [speclj.core :refer :all]))

(describe "what can i add to a node"
  (it "knows about juxtaposition rules"
    (let [rules {:root {:type :juxtaposition :children [:first :middle]}
                 :first {:type :character :children ["a"]}
                 :middle {:type :character :children ["z"]}}]
      (should=
        [{:rule (build-rule-with-name :middle rules) :children []}]
        (addable-children
          ""
          rules
          {:rule (build-rule-with-name :root rules)
           :children [{:rule (build-rule-with-name :first rules) :children ["a"]}]}))))

  (it "knows about character rules"
    (let [rules {:root {:type :character :children ["a"]}}]
      (should=
        ["a"]
        (addable-children
          "a"
          rules
          {:rule (build-rule-with-name :root rules) :children []}))
      (should=
        []
        (addable-children
          "this program does not have an 'a' at the start"
          rules
          {:rule (build-rule-with-name :root rules) :children []}))
      (should=
        []
        (addable-children
          "aaaaaaa"
          rules
          {:rule (build-rule-with-name :root rules) :children ["a"]}))))

  (it "knows about repetition rules"
    (let [rules {:root {:type :repetition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should=
        [{:rule (build-rule-with-name :char-rule rules) :children []}]
        (addable-children
          ""
          rules
          {:rule (build-rule-with-name :root rules) :children [{:rule (build-rule-with-name :char-rule rules) :children ["Z"]}]}))))

  (it "knows about exclusion rules"
    (let [rules {:root {:type :exclusion :children ["("]}}]
      (should=
        ["z"]
        (addable-children
          "zoo"
          rules
          {:rule (build-rule-with-name :root rules) :children []}))
      (should=
        []
        (addable-children
          "(zoo"
          rules
          {:rule (build-rule-with-name :root rules) :children []}))))

  (it "wont let you add anything to a node whose last child is not closable"
    (let [rules {:root {:type :repetition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should=
        []
        (addable-children
          "ZZZZ"
          rules
          {:rule (build-rule-with-name :root rules) :children [{:rule (build-rule-with-name :char-rule rules) :children []}]})))))

(describe "closable?"
  (it "A juxtaposition is not closeable unless its stuff is all finished"
    (let [rules {:root {:type :juxtaposition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should (closeable?
              rules
              {:rule (build-rule-with-name :root rules) :children [{:rule (build-rule-with-name :char-rule rules) :children ["Z"]}]}))
    (should-not (closeable?
                  rules
                  {:rule (build-rule-with-name :root rules) :children []}))))
  (it "a character rule is not closeable unless it has a child"
    (let [rules {:root {:type :character :children ["A"]}}]
      (should
      (closeable?
        rules
        {:rule (build-rule-with-name :root rules) :children ["A"]}))
    (should-not
      (closeable?
        rules
        {:rule (build-rule-with-name :root rules) :children []}))))
  (it "a repetition rule is always closeable"
    (let [rules {:root {:type :repetition :children [:wat]}}]
      (should
        (closeable?
          rules
          {:rule (build-rule-with-name :root rules) :children []}))))
  (it "an exclusion rule is not closeable unless it has a child"
    (let [rules {:root {:type :exclusion :children ["("]}}]
      (should
        (closeable?
          rules
          {:rule (build-rule-with-name :root rules) :children ["Z"]}))
      (should-not
        (closeable?
          rules
          {:rule (build-rule-with-name :root rules) :children []}))))
  (it "a node is not closeable unless its last child is also closeable"
    (let [rules {:root {:type :juxtaposition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should-not
        (closeable?
          rules
          {:rule (build-rule-with-name :root rules) :children [{:rule (build-rule-with-name :char-rule rules) :children []}]})))))

(describe "integration"
  (it "...works?"
    (let [rules {:root {:type :juxtaposition :children [:a-char :b-char :repeat-root :a-char]}
                 :a-char {:type :character :children ["a"]}
                 :b-char {:type :character :children ["b"]}
                 :repeat-root {:type :repetition :children [:root]}}
          program "abababa"]
      (should-not (empty? (get-parse-tree rules program)))))

  (it "does easy lisp"
    (let [rules {:root {:type :juxtaposition :children [:open-paren
                                                        :non-parens
                                                        :repeat-root
                                                        :non-parens
                                                        :close-paren
                                                        :non-parens]}
                 :open-paren {:type :character :children ["("]}
                 :close-paren {:type :character :children [")"]}
                 :repeat-root {:type :repetition :children [:root]}
                 :non-paren {:type :exclusion :children ["(" ")"]}
                 :non-parens {:type :repetition :children [:non-paren]}
                 }
          program "(+ 1 2)"]
      (should-not (empty? (get-parse-tree rules program)))))

  (it "does harder lisp"
    (let [rules {:root {:type :juxtaposition :children [:open-paren
                                                        :non-parens
                                                        :repeat-root
                                                        :non-parens
                                                        :close-paren
                                                        :non-parens]}
                 :open-paren {:type :character :children ["("]}
                 :close-paren {:type :character :children [")"]}
                 :repeat-root {:type :repetition :children [:root]}
                 :non-paren {:type :exclusion :children ["(" ")"]}
                 :non-parens {:type :repetition :children [:non-paren]}
                 }
          program "(+ 1 (* 3 4) (* 2 3))"]
      (should-not (empty? (get-parse-tree rules program)))))

  )
