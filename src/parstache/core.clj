(ns parstache.core
  (:require
    [parstache.parser-generator :refer :all]
    [clojure.walk :as walk]))

(declare render-parsed render)

(def mustache-specification
  {:root {:type :repetition :repeated-rule-name :form}
   :form {:type :or :allowed-rules [:substitution
                                    :subcontext
                                    :partial
                                    :many-non-mustaches]}
   :non-mustaches {:type :repetition :repeated-rule-name :non-bracket}
   :many-non-mustaches {:type :juxtaposition :required-children [:non-bracket :non-mustaches]}
   :non-bracket {:type :exclusion :unpossible-characters ["{" "}"]}
   :non-special {:type :exclusion :unpossible-characters ["#" "/" ">"]}
   :substitution {:type :juxtaposition :required-children [:open-stache :non-special :non-mustaches :close-stache]}
   :open-stache {:type :juxtaposition :required-children [{:type :character :possible-characters ["{"]}
                                                                 {:type :character :possible-characters ["{"]}]}
   :close-stache {:type :juxtaposition :required-children [{:type :character :possible-characters ["}"]}
                                                                  {:type :character :possible-characters ["}"]}]}
   :subcontext {:type :juxtaposition :required-children [:start-subcontext :root :end-subcontext]}
   :start-subcontext {:type :juxtaposition :required-children [:open-stache
                                                               {:type :character :possible-characters ["#"]}
                                                               :non-mustaches
                                                               :close-stache]}
   :partial {:type :juxtaposition :required-children [:open-stache
                                                      {:type :character :possible-characters [">"]}
                                                      :non-mustaches
                                                      :close-stache]}
   :end-subcontext {:type :juxtaposition :required-children [:open-stache
                                                             {:type :character :possible-characters ["/"]}
                                                             :non-mustaches
                                                             :close-stache]}})

(defn homebrew-parse [document] (:tree (get-parse-tree mustache-specification document)))

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
            subdata (get data context-field)]
        (if (sequential? subdata)
          (mapcat #(render-parsed inner-form % partials) subdata)
          (if subdata (render-parsed inner-form subdata partials) "")))
      :partial
      (let [partial-name (string-leaves (get (:children parsed) 2))]
        (render (get partials partial-name) data partials)))))

(defn render
  ([document data]
   (render document data {}))
  ([document data partials]
   (render-parsed (homebrew-parse document) data partials)))
