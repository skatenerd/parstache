(ns parstache.core
  (:require
    [instaparse.core :as instaparse]
    [parstache.parser-generator :refer :all]
    [clojure.walk :as walk]))

(declare render-parsed)

(def ebnf
  "DOCUMENT := (RAW | SUBSTITUTION | SUBCONTEXT | PARTIAL)+
  RAW := #'[^\\{\\}]+'
  SUBSTITUTION := <'{{'> RAW <'}}'>
  START_SUBCONTEXT := <'{{#'> RAW <'}}'>
  END_SUBCONTEXT := <'{{/'> RAW <'}}'>
  PARTIAL := <'{{>'> RAW <'}}'>
  SUBCONTEXT :=  START_SUBCONTEXT DOCUMENT END_SUBCONTEXT")

(def mustache-specification
  {:root {:type :repetition :repeated-rule-name :form}
   :form {:type :or :allowed-rules [:substitution
                                    :subcontext
                                    :partial
                                    :many-non-mustaches]}
   :non-mustaches {:type :repetition :repeated-rule-name :non-bracket}
   :many-non-mustaches {:type :juxtaposition :required-children [:non-bracket :non-mustaches]}
   :non-bracket {:type :exclusion :unpossible-characters ["{" "}"]}
   :non-pound-slash {:type :exclusion :unpossible-characters ["#" "/"]}
   :substitution {:type :juxtaposition :required-children [:double-open-stache :non-pound-slash :non-mustaches :double-close-stache]}
   :double-open-stache {:type :juxtaposition :required-children [:open-stache :open-stache]}
   :double-close-stache {:type :juxtaposition :required-children [:close-stache :close-stache]}
   :open-stache {:type :character :possible-characters ["{"]}
   :close-stache {:type :character :possible-characters ["}"]}
   :subcontext {:type :juxtaposition :required-children [:start-subcontext :root :end-subcontext]}
   :start-subcontext {:type :juxtaposition :required-children [:double-open-stache :pound :non-mustaches :double-close-stache]}
   :pound {:type :character :possible-characters ["#"]}
   :slash {:type :character :possible-characters ["/"]}
   :gator {:type :character :possible-characters [">"]}
   :partial {:type :juxtaposition :required-children [:double-open-stache :gator :non-mustaches :double-close-stache]}
   :end-subcontext {:type :juxtaposition :required-children [:double-open-stache :slash :non-mustaches :double-close-stache]}})

(def parse (instaparse/parser ebnf))

(defn homebrew-parse [document] (:tree (get-parse-tree mustache-specification document)))

(defn- render-subcontext [children data partials]
  (let [key-name (last (last (first children)))
        body (second children)
        subdata (get data key-name)]
    (cond
      (sequential? subdata)
      (map #(render-parsed body % partials) subdata)
      subdata
      (render-parsed body subdata partials))))

(defn- render-partial [children data partials]
  (let [partial-name (last (first children))
        partial-contents (get partials partial-name)
        parsed (parse partial-contents)]
    (render-parsed parsed data partials)))

(defn- render-parsed [parsed data partials]
  (let [node-type (:name parsed)
        children (:children parsed)
        data (walk/stringify-keys data)
        partials (walk/stringify-keys partials)]
    (case node-type
      :root
      (apply str (mapcat #(render-parsed % data partials) children))
      :form
      (mapcat #(render-parsed % data partials) children)
      :many-non-mustaches
      (string-leaves parsed)
      :substitution
      (let [without-mustaches (update-in parsed [:children] #(drop 1 (drop-last %)))
            contents (string-leaves without-mustaches)]
        (get data contents))
      :subcontext
      (let [opening-tag (-> parsed :children first)
            without-mustaches (update-in opening-tag [:children] #(drop 2 (drop-last %)))
            context-field (string-leaves without-mustaches)
            inner-form (second (:children parsed))
            inside-subcontext (string-leaves inner-form)]
        "ZZZZZZZZZZZZZZ"
        ))))

(defn render
  ([document data]
   (render document data {}))
  ([document data partials]
   (render-parsed (homebrew-parse document) data partials)
  ;(render-parsed (parse document) data partials)


   ))
