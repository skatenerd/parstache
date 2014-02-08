(ns parstache.parser-generator-spec
  (:require
    [parstache.parser-generator :refer :all]
    [speclj.core :refer :all]))

(describe "possible mutations"
  (it "finds the obvious rule-add mutation"
    (let [intermediate-parse-tree {:type :root :children [{:type :child :children ["HOWDY!"]}]}
          root intermediate-parse-tree
          child (first (:children intermediate-parse-tree))
          legally-addable-children {root [{:type :child :children ["YODOG"]}] child ["BIBIMBAP"]}
          possible-new-trees [{:type :root :children [{:type :child :children ["HOWDY!"]} {:type :child :children ["YODOG"]}]}
                              {:type :root :children [{:type :child :children ["HOWDY!" "BIBIMBAP"]}]}]]
      (should==
        possible-new-trees
        (add-to-tree intermediate-parse-tree legally-addable-children)))))

;(describe "generate a parser"
;  (it "parses aaa"
;    (should=
;      {:rule "REPEAT_A" :contents ["a" "a" "a"]}
;      ((generate-parser
;       [{:name "REPEAT_A" :type :string-repetition :value "a"}])
;         "aaa"
;         ))))
