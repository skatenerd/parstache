(ns parstache.vectors)

(defn update-last-element [v new-element]
  (assoc-in v [(dec (count v))] new-element))
