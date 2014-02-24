(ns parstache.tree-spec
  (:require
    [parstache.tree :refer :all]
    [speclj.core :refer :all]))


(defn euclidean-distance [p1 p2]
  (let [component-differences (map - p1 p2)
        squared-components (map #(* % %) component-differences)
        total-squared-components (reduce + squared-components)]
    (Math/sqrt total-squared-components)))

;thanks to cristophe grand for the testing idea

(describe "best-first"
  (it "finds the first matching node, greedily"
    (let [point-graph {[0 0] #{[0 5] [1 9]}
                       [0 5] #{[0 11]}
                       [1 5] #{[0 99]}}]
      (should=
        [0 11]
        (best-first
          [0 0]
          #(- 10 (second %))
          #(euclidean-distance [0 0] %)
          #(> (second %) 10)
          #(get point-graph %)))))

  (it "falls back to a less-greedy guy if its the only way to get there"
    (let [point-graph {[0 0] #{[0 5] [1 5]}
                       [0 5] #{[0 11]}
                       [1 5] #{[0 99]}}]
      (should=
        [0 99]
        (best-first
          [0 0]
          #(- 10 (second %))
          #(euclidean-distance [0 0] %)
          #(> (second %) 20)
          #(get point-graph %))))))
