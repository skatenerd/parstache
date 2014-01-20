(ns parstache.core
  (:require
    [instaparse.core :as instaparse]))

(declare render)


(def ebnf
  "DOCUMENT := (RAW | SUBSTITUTION | SUBCONTEXT)+
  RAW := #'[^\\{\\}]+'
  SUBSTITUTION := <'{{'> RAW <'}}'>
  START_SUBCONTEXT := <'{{#'> RAW <'}}'>
  END_SUBCONTEXT := <'{{/'> RAW <'}}'>
  SUBCONTEXT :=  START_SUBCONTEXT DOCUMENT END_SUBCONTEXT")

(def parse (instaparse/parser ebnf))

(defn raw? [subdocument]
  (= (first subdocument) :RAW))

(defn render-subcontext [children data]
  (let [key-name (last (last (first children)))
        body (second children)
        subdata (get data key-name)]
    (map #(render body %) subdata)))

(defn render [document data]
  (if (nil? document)
    "NONONONO"
    (let [type (first document)
          stuff (rest document)]
      (cond
        (= :DOCUMENT type)
        (apply str (apply concat (map #(render % data) stuff)))
        (= :RAW type)
        (first stuff)
        (= :SUBSTITUTION type)
        (let [key (last (first stuff))]
          (get data key))
        (= :SUBCONTEXT type)
        (render-subcontext stuff data)
        :else
        (recur (first stuff) {})))))
