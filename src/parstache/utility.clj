(ns parstache.utility)

(defn concatv [& args] (into [] (apply concat args)))
