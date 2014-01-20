(ns parstache.core-spec
  (:require [speclj.core :refer :all]
            [parstache.core :refer :all]))

(describe "parsing"
  (it "parses substitutions"
    (should=
      [:DOCUMENT [:RAW "foo"]
       [:SUBSTITUTION [:RAW "abc"]]
       [:RAW "bar"]]
      (parse "foo{{abc}}bar")))

  (it "parses subcontexts"
    (should=
      [:DOCUMENT
       [:RAW "customers: "]
       [:SUBCONTEXT
        [:START_SUBCONTEXT [:RAW "customers"]]
        [:DOCUMENT [:SUBSTITUTION [:RAW "wat"]]]
        [:END_SUBCONTEXT [:RAW "customers"]]]
       [:RAW "thank you"]]
      (parse "customers: {{#customers}}{{wat}}{{/customers}}thank you"))))
