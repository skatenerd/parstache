(ns parstache.parser-generator.ebnf-spec
  (:require
    [parstache.parser-generator.ebnf :refer :all]
    clojure.pprint
    [parstache.parser-generator :refer :all]
    [speclj.core :refer :all]))

(def ebnf-ebnf
  "LETTER = \"A\" | \"B\" | \"C\" | \"D\" | \"E\" | \"F\" | \"G\"
| \"H\" | \"I\" | \"J\" | \"K\" | \"L\" | \"M\" | \"N\"
| \"O\" | \"P\" | \"Q\" | \"R\" | \"S\" | \"T\" | \"U\"
| \"V\" | \"W\" | \"X\" | \"Y\" | \"Z\" ;

DIGIT = \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\" ;

SYMBOL = \"[\" | \"]\" | \"{\" | \"}\" | \"(\" | \")\" | \"<\" | \">\"
| \"'\" | \"\"\" | \"=\" | \"|\" | \".\" | \",\" | \";\" ;

RHS = IDENTIFIER
| TERMINAL
| \"[\" , RHS , \"]\"
| \"{\" , RHS , \"}\"
| \"(\" , RHS , \")\"
| RHS , \"|\" , RHS
| RHS , \",\" , RHS ;

CHARACTER = LETTER | DIGIT | SYMBOL | \"_\" ;

IDENTIFIER = LETTER , { LETTER | DIGIT | \"_\" } ;

TERMINAL = \"'\" , CHARACTER , { CHARACTER } , \"'\"
| \"\"\" , CHARACTER , { CHARACTER } , \"\"\" ;

LHS = IDENTIFIER ;

RULE = LHS , \"=\" , RHS , \";\" ;

GRAMMAR = { RULE };")


(defn eliminate-character [s to-eliminate]
  (clojure.string/replace s to-eliminate ""))

(describe "ebnf"
  (xit "accurately describes ebnf"
;this is a very slow test.
    (let [preprocessed-ebnf (-> ebnf-ebnf (eliminate-character " ") (eliminate-character "\n"))
          compiled-grammar (compile-grammar ebnf-rules)]
      (get-parse-tree compiled-grammar preprocessed-ebnf))))
