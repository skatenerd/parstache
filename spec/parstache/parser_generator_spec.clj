(ns parstache.parser-generator-spec
  (:require
    [parstache.parser-generator :refer :all]
    [speclj.core :refer :all]))

(describe "add to parse tree"
  (it "finds all additions according to whatever the legally-addable-children function says to do"
    (let [intermediate-parse-tree {:type :root :children [{:type :child :children ["HOWDY!"]}]}
          root intermediate-parse-tree
          child (first (:children intermediate-parse-tree))
          legally-addable-children {root [{:type :child :children ["YODOG"]}] child ["BIBIMBAP"]}
          possible-new-trees [{:type :root :children [{:type :child :children ["HOWDY!"]} {:type :child :children ["YODOG"]}]}
                              {:type :root :children [{:type :child :children ["HOWDY!" "BIBIMBAP"]}]}]]
      (should==
        possible-new-trees
        (add-to-tree intermediate-parse-tree legally-addable-children)))))

(describe "what can i add to a node"
  (it "knows about juxtaposition rules"
    (let [rules {:root {:type :juxtaposition :children [:first :middle]}
                 :first {:type :character :children ["a"]}
                 :middle {:type :character :children ["z"]}}]
      (should=
        [{:name :middle :type :character :children []}]
        (addable-children
          ""
          rules
          {:name :root :type :juxtaposition :children [{:name :first :type :character :children ["a"]}]}))))

  (it "knows about character rules"
    (let [rules {:root {:type :character :children ["a"]}}]
      (should=
        ["a"]
        (addable-children
          "a"
          rules
          {:name :root :type :character :children []}))
      (should=
        []
        (addable-children
          "this program does not have an 'a' at the start"
          rules
          {:name :root :type :character :children []}))))

  (it "knows about repetition rules"
    (let [rules {:root {:type :repetition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should=
        [{:name :char-rule :type :character :children []}]
        (addable-children
          ""
          rules
          {:name :root :type :repetition :children [{:name :char-rule :type :character :children ["Z"]}]}))))

  (it "wont let you add anything to a node whose last child is not closable"
    (let [rules {:root {:type :repetition :children [:char-rule]}
                 :char-rule {:type :character :children ["Z"]}}]
      (should=
        []
        (addable-children
          ""
          rules
          {:name :root :type :repetition :children [{:name :char-rule :type :character :children []}]})))
    )


  )

(describe "closable?"
  (it "A juxtaposition is not closeable unless its stuff is all finished"
    (should (closeable?
              {:root {:type :juxtaposition :children [:char-rule]}
               :char-rule {:type :character :children ["Z"]}}
              {:name :root :type :juxtaposition :children [{:name :char-rule :type :character :children ["Z"]}]}
              ))
    (should-not (closeable?
                  {:root {:type :juxtaposition :children [:char-rule]}
                   :char-rule {:type :character :children ["Z"]}}
                  {:name :root :type :juxtaposition :children []})))
  (it "a character rule is not closeable unless it has a child"
    (should
      (closeable?
        {:root {:type :character :children ["A"]}}
        {:name :root :type :character :children ["A"]}))
    (should-not
      (closeable?
        {:root {:type :character :children ["A"]}}
        {:name :root :type :character :children []})))
  (it "a repetition rule is always closeable"
    (should
      (closeable?
        {:root {:type :repetition :children [:wat]}}
        {:name :root :type :repetiition :children []})))
  (it "nil is always closeable"
    (should
      (closeable? {} nil)))
  (it "a node is not closeable unless its last child is also closeable"
    (should-not
      (closeable?
        {:root {:type :juxtaposition :children [:char-rule]}
         :char-rule {:type :character :children ["Z"]}}
        {:name :root :type :juxtaposition :children [{:name :char-rule :type :character :children []}]}))))
