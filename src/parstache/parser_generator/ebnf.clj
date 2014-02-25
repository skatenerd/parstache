(ns parstache.parser-generator.ebnf
  (:require [parstache.utility :refer [concatv]]))

(def letters
  ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"])

(def numbers
  (map str (range 10)))

(def special-characters
  ["[" "]" "{" "}" "(" ")" "<" ">" "'" "\"" "=" "|" "." "," ";"])


(def identifier-characters (concatv letters numbers ["_"]))

(def characters (concatv identifier-characters special-characters))

(defn character-rule [allowed] {:type :character :possible-characters [allowed]})

(def ebnf-rules
  {:root {:type :repetition :repeated-rule :rule}
   :rule {:type :juxtaposition :required-children [:lhs
                                                   (character-rule "=")
                                                   :rhs
                                                   :semicolon]}
   :identifier-character {:type :character :possible-characters identifier-characters}
   :identifier {:type :one-or-more :repeated-rule :identifier-character}
   :semicolon (character-rule ";")
   :lhs {:type :juxtaposition :required-children [:identifier]}
   :rhs {:type :or :allowed-grammar [:identifier
                                     :terminal
                                     :option-rule
                                     :repetition-rule
                                     :grouping-rule
                                     :pipe-concatenation
                                     :comma-concatenation]}
   :quotation {:type :character :possible-characters ["\"" "'"]}
   :non-quote {:type :exclusion :unpossible-characters ["\"" "'"]}
   :character {:type :character :possible-characters characters}
   :terminal {:type :juxtaposition :required-children [:quotation
                                                       {:type :one-or-more :repeated-rule :character}
                                                       :quotation]}
   :option-rule {:type :juxtaposition :required-children [(character-rule "[") :rhs (character-rule "]")]}
   :repetition-rule {:type :juxtaposition :required-children [(character-rule "{") :rhs (character-rule "}")]}
   :grouping-rule {:type :juxtaposition :required-children [(character-rule "(") :rhs (character-rule ")")]}
   :comma-concatenation {:type :juxtaposition :required-children [:rhs (character-rule ",") :rhs]}
   :pipe-concatenation {:type :juxtaposition :required-children [:rhs (character-rule "|") :rhs]}})
