(ns parstache.core
  (:require [instaparse.core :as instaparse]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(def ebnf
  "DOCUMENT := (RAW | SUBSTITUTION | SUBCONTEXT)+
  RAW := #'[^\\{\\}]+'
  SUBSTITUTION := <'{{'> RAW <'}}'>
  START_SUBCONTEXT := <'{{#'> RAW <'}}'>
  END_SUBCONTEXT := <'{{/'> RAW <'}}'>
  SUBCONTEXT :=  START_SUBCONTEXT DOCUMENT END_SUBCONTEXT")

(def parse (instaparse/parser ebnf))

