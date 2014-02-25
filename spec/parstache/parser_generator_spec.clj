(ns parstache.parser-generator-spec
  (:require
    [parstache.core :refer [mustache-specification]]
    [parstache.parser-generator :refer :all]
    [clojure.pprint]
    [parstache.parser-generator.nodes :refer :all]
    [speclj.core :refer :all]))

(context "rule compiling"
  (it "compiles empty rules"
    (let [to-compile {}
          expected {}]
      (should= expected (compile-grammar to-compile))))

  (it "compiles 'word' rules"
    (let [to-compile {:root {:type :word :allowed "YO"}}
          expected {:root {:type :juxtaposition
                           :required-children [{:type :character :possible-characters ["Y"]}
                                               {:type :character :possible-characters ["O"]}]}}]

      (should= expected (compile-grammar to-compile))))

  (it "compiles 'one-or-more' rules"
    (let [to-compile {:root {:type :one-or-more :repeated-rule :foo}
                      :foo {:type :character :dont :care}}
          expected {:root {:type :juxtaposition
                           :required-children [:foo
                                               {:type :repetition :repeated-rule :foo}]}
                    :foo {:type :character :dont :care}}]
      (should= expected (compile-grammar to-compile)))))

(describe "integration"
  (it "parses simple recursive grammar"
    (let [grammar {:root {:type :juxtaposition :required-children [:a-char
                                                                   :b-char
                                                                   :repeat-root
                                                                   :a-char]}
                   :a-char {:type :character :possible-characters ["a"]}
                   :b-char {:type :character :possible-characters ["b"]}
                   :repeat-root {:type :repetition :repeated-rule :root}}
          program "ababaa"]
      (should-not (empty? (get-parse-tree grammar program)))))

  (it "does lisp"
    (let [grammar {:root {:type :juxtaposition :required-children [:open-paren
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
          program "(+ 1 2 (* 3 4) (* 2 3))"]
      (should= program (string-leaves (:tree (get-parse-tree grammar program))))))

  (it "does mustache"
    (let [grammar (compile-grammar mustache-specification)
          program "bro {{#foo}}{{hi}}{{/foo}}"]
      (should= program (string-leaves (:tree (get-parse-tree grammar program))))))


  (it "supports nested rules"
    (let [grammar {:root {:type :juxtaposition
                        :required-children [{:type :character
                                             :possible-characters ["a"]}
                                            {:type :character
                                             :possible-characters ["b"]}
                                            :repeat-root
                                            {:type :character
                                             :possible-characters ["a"]}]}

                 :repeat-root {:type :repetition :repeated-rule :root}}
          program "abababa"]
      (should-not (empty? (get-parse-tree grammar program))))))
