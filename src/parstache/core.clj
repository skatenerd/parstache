(ns parstache.core
  (:require
    [instaparse.core :as instaparse]
    [clojure.walk    :as walk]))

(declare render-parsed)

(def ebnf
  "DOCUMENT := (RAW | SUBSTITUTION | SUBCONTEXT | PARTIAL)+
  RAW := #'[^\\{\\}]+'
  SUBSTITUTION := <'{{'> RAW <'}}'>
  START_SUBCONTEXT := <'{{#'> RAW <'}}'>
  END_SUBCONTEXT := <'{{/'> RAW <'}}'>
  PARTIAL := <'{{>'> RAW <'}}'>
  SUBCONTEXT :=  START_SUBCONTEXT DOCUMENT END_SUBCONTEXT")

(def parse (instaparse/parser ebnf))

(def otherbnf
  "R=A B R* A
  AC = 'a'
  A = AC+
  BC='b'
  B=BC+")
(def fakeparse (instaparse/parser otherbnf))
(prn (fakeparse "aabbaabbaa"))

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
  (let [type (first parsed)
        children (rest parsed)
        data (walk/stringify-keys data)
        partials (walk/stringify-keys partials)]
    (case type
      :DOCUMENT
      (apply str (apply concat (map #(render-parsed % data partials) children)))
      :RAW
      (first children)
      :SUBSTITUTION
      (let [key (last (first children))]
        (get data key))
      :SUBCONTEXT
      (render-subcontext children data partials)
      :PARTIAL
      (render-partial children data partials)
      (recur (first children) {} {}))))

(defn render
  ([document data]
   (render document data {}))
  ([document data partials]
  (render-parsed (parse document) data partials)))
