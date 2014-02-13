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

;(describe "what can i add to a node"
;  (it "knows about juxtaposition rules"
;    (let [rules {:root {:type :juxtaposition :children [:first :middle]}
;                 :first {:type :character :children ["a"]}
;                 :middle {:type :character :children ["z"]}}]
;      (should=
;        [{:name :middle :type :character :children []}]
;        (addable-children
;          ""
;          rules
;          {:name :root :type :juxtaposition :children [{:name :first :type :character :children ["a"]}]}))))
;
;  (it "knows about character rules"
;    (let [rules {:root {:type :character :children ["a"]}}]
;      (should=
;        ["a"]
;        (addable-children
;          "a"
;          rules
;          {:name :root :type :character :children []}))
;      (should=
;        []
;        (addable-children
;          "this program does not have an 'a' at the start"
;          rules
;          {:name :root :type :character :children []}))
;      (should=
;        []
;        (addable-children
;          "aaaaaaa"
;          rules
;          {:name :root :type :character :children ["a"]}))))
;
;  (it "knows about repetition rules"
;    (let [rules {:root {:type :repetition :children [:char-rule]}
;                 :char-rule {:type :character :children ["Z"]}}]
;      (should=
;        [{:name :char-rule :type :character :children []}]
;        (addable-children
;          ""
;          rules
;          {:name :root :type :repetition :children [{:name :char-rule :type :character :children ["Z"]}]}))))
;
;  (it "knows about exclusion rules"
;    (let [rules {:root {:type :exclusion :children ["("]}}]
;      (should=
;        ["z"]
;        (addable-children
;          "zoo"
;          rules
;          {:name :root :type :exclusion :children []}))
;      (should=
;        []
;        (addable-children
;          "(zoo"
;          rules
;          {:name :root :type :exclusion :children []}))))
;
;  (it "wont let you add anything to a node whose last child is not closable"
;    (let [rules {:root {:type :repetition :children [:char-rule]}
;                 :char-rule {:type :character :children ["Z"]}}]
;      (should=
;        []
;        (addable-children
;          ""
;          rules
;          {:name :root :type :repetition :children [{:name :char-rule :type :character :children []}]})))))
;
;(describe "closable?"
;  (it "A juxtaposition is not closeable unless its stuff is all finished"
;    (should (closeable?
;              {:root {:type :juxtaposition :children [:char-rule]}
;               :char-rule {:type :character :children ["Z"]}}
;              {:name :root :type :juxtaposition :children [{:name :char-rule :type :character :children ["Z"]}]}
;              ))
;    (should-not (closeable?
;                  {:root {:type :juxtaposition :children [:char-rule]}
;                   :char-rule {:type :character :children ["Z"]}}
;                  {:name :root :type :juxtaposition :children []})))
;  (it "a character rule is not closeable unless it has a child"
;    (should
;      (closeable?
;        {:root {:type :character :children ["A"]}}
;        {:name :root :type :character :children ["A"]}))
;    (should-not
;      (closeable?
;        {:root {:type :character :children ["A"]}}
;        {:name :root :type :character :children []})))
;  (it "a repetition rule is always closeable"
;    (should
;      (closeable?
;        {:root {:type :repetition :children [:wat]}}
;        {:name :root :type :repetition :children []})))
;  (it "an exclusion rule is not closeable unless it has a child"
;    (should
;      (closeable?
;        {:root {:type :exclusion :children ["("]}}
;        {:name :root :type :exclusion :children ["Z"]}))
;    (should-not
;      (closeable?
;        {:root {:type :exclusion :children ["A"]}}
;        {:name :root :type :exclusion :children []})))
;  (it "nil is always closeable"
;    (should
;      (closeable? {} nil)))
;  (it "a node is not closeable unless its last child is also closeable"
;    (should-not
;      (closeable?
;        {:root {:type :juxtaposition :children [:char-rule]}
;         :char-rule {:type :character :children ["Z"]}}
;        {:name :root :type :juxtaposition :children [{:name :char-rule :type :character :children []}]}))))

(describe "integration"
  (it "...works?"
    (let [rules {:root {:type :juxtaposition :children [:a-char :b-char :repeat-root :a-char]}
                 :a-char {:type :character :children ["a"]}
                 :b-char {:type :character :children ["b"]}
                 :repeat-root {:type :repetition :children [:root]}}
          program "abababa"]
      (should-not (empty? (get-parse-trees rules program)))))

  (it "does lisp?"
    (let [rules {:root {:type :juxtaposition :children [:open-paren
                                                        :non-parens
                                                        :repeat-root
                                                        :non-parens
                                                        :close-paren]}
                 :open-paren {:type :character :children ["("]}
                 :close-paren {:type :character :children [")"]}
                 :repeat-root {:type :repetition :children [:root]}
                 :non-paren {:type :exclusion :children ["(" ")"]}
                 :non-parens {:type :repetition :children [:non-paren]}
                 }
          program "(+ 1 2 (* 5 3) 2)"]
      (clojure.pprint/pprint (first (get-parse-trees rules program)))
      (should-not (empty? (get-parse-trees rules program))))))
