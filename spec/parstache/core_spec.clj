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
      (parse "customers: {{#customers}}{{wat}}{{/customers}}thank you")))

  (it "parses nested subcontexts"
    (should=
      [:DOCUMENT
       [:RAW "customers: "]
       [:SUBCONTEXT
        [:START_SUBCONTEXT [:RAW "parents"]]
        [:DOCUMENT
         [:SUBCONTEXT
          [:START_SUBCONTEXT [:RAW "children"]]
          [:DOCUMENT [:SUBSTITUTION [:RAW "name"]]]
          [:END_SUBCONTEXT [:RAW "children"]]]]
        [:END_SUBCONTEXT [:RAW "parents"]]]]
      (parse "customers: {{#parents}}{{#children}}{{name}}{{/children}}{{/parents}}"))))

(describe "rendering"
  (it "renders flat strings"
    (should=
      "hello"
      (render "hello" {})))

  (it "renders simple lookups strings"
    (should=
      "hello Joe, how are you doing?  i am doing well"
      (render "hello {{name}}, how are you doing?  i am doing {{status}}" {"name" "Joe" "status" "well"})))

  (it "loops over collections"
    (should=
      "hello, james, frank"
      (render "hello{{#people}}, {{name}}{{/people}}" {"people" [{"name" "james"} {"name" "frank"}]})))

  (it "loops over nested collections"
    (should=
      "immediate families are:  laura: logan katie linda: rutledge walter "
      (render "immediate families are:  {{#parents}}{{name}}: {{#children}}{{name}} {{/children}}{{/parents}}" {"parents" [{"name" "laura"
                                                                                                                      "children" [{"name" "logan"} {"name" "katie"}]}
                                                                                                                     {"name" "linda"
                                                                                                                      "children" [{"name" "rutledge"} {"name" "walter"}]}]})))
  (it "makes fake conditionals, thereby implementing logic in the view, so mustache is really basically pointless"
    (should=
      "i have a son, jeremy"
      (render "i have a {{#son}}son, {{name}}{{/son}}{{#daughter}}daughter, {{name}}{{/daughter}}" {"son" {"name" "jeremy"}}))))


