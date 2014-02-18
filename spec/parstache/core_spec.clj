(ns parstache.core-spec
  (:require [speclj.core :refer :all]
            [parstache.core :refer :all]))

(describe "rendering"
  (it "renders flat strings"
    (should=
      "hello"
      (render "hello" {})))

  (it "renders simple lookups strings"
    (should=
      "hello Joe, how are you doing?  i am doing well"
      (render "hello {{name}}, how are you doing?  i am doing {{status}}" {"name" "Joe" "status" "well"})))

  (it "supports keywordized data"
    (should=
      "hello Joe, how are you doing?  i am doing well"
      (render "hello {{name}}, how are you doing?  i am doing {{status}}" {:name "Joe" :status "well"})))

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
      (render "i have a {{#son}}son, {{name}}{{/son}}{{#daughter}}daughter, {{name}}{{/daughter}}" {"son" {"name" "jeremy"}})))

  (xit "renders with partials"
    (should=
      "yo, do you want to hear about my kid?  sweet!  his name is joe"
      (render "yo, do you want to hear about my kid?  sweet!  {{#kid}}{{>kidpartial}}{{/kid}}" {"kid" {"name" "joe"}} {"kidpartial" "his name is {{name}}"})
      )
    )
  (xit "supports keywordized partial names"
    (should=
      "yo, do you want to hear about my kid?  sweet!  his name is joe"
      (render "yo, do you want to hear about my kid?  sweet!  {{#kid}}{{>kidpartial}}{{/kid}}" {"kid" {"name" "joe"}} {:kidpartial "his name is {{name}}"}))))


