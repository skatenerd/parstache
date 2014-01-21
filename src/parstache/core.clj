(ns parstache.core
  (:require
    [instaparse.core :as instaparse]))

(declare render-parsed)


(def ebnf
  "DOCUMENT := (RAW | SUBSTITUTION | SUBCONTEXT)+
  RAW := #'[^\\{\\}]+'
  SUBSTITUTION := <'{{'> RAW <'}}'>
  START_SUBCONTEXT := <'{{#'> RAW <'}}'>
  END_SUBCONTEXT := <'{{/'> RAW <'}}'>
  SUBCONTEXT :=  START_SUBCONTEXT DOCUMENT END_SUBCONTEXT")

(def parse (instaparse/parser ebnf))

(defn render-subcontext [children data]
  (let [key-name (last (last (first children)))
        body (second children)
        subdata (get data key-name)]
    (cond
      (sequential? subdata)
      (map #(render-parsed body %) subdata)
      subdata
      (render-parsed body subdata))))

(defn- render-parsed [parsed data]
  (let [type (first parsed)
        children (rest parsed)]
    (case type
      :DOCUMENT
      (apply str (apply concat (map #(render-parsed % data) children)))
      :RAW
      (first children)
      :SUBSTITUTION
      (let [key (last (first children))]
        (get data key))
      :SUBCONTEXT
      (render-subcontext children data)
      (recur (first children) {}))))

(defn render [document data]
  (render-parsed (parse document) data))
