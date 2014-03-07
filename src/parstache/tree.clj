(ns parstache.tree
  (:require
    clojure.pprint)
  (:import (java.util.concurrent PriorityBlockingQueue Executors)))

(defn frontier-additions [cost heuristic get-neighbors best-choice]
  (map (fn [neighbor] {:node neighbor
                       :cost (cost neighbor)
                       :heuristic (heuristic neighbor)})
       (get-neighbors (:node best-choice))))

(defn- new-frontier [frontier best-frontier-choice get-neighbors heuristic cost]
  (let [best-removed (disj frontier best-frontier-choice)
        to-add (frontier-additions cost heuristic get-neighbors best-frontier-choice)]
    (if (empty? to-add)
      best-removed
      (apply conj best-removed to-add))))

(defn best-choice [frontier]
  (first frontier))

(defn compare-states [element-1 element-2]
  (let [[total-cost-1 total-cost-2] (map #(+ (:cost %) (:heuristic %)) [element-1 element-2])]
    (cond
      (= (:node element-1) (:node element-2))
      0
      (> total-cost-1 total-cost-2)
      1
      :else
      -1)))

(def empty-frontier (sorted-set-by compare-states))

(defn best-first-private [frontier heuristic cost stopping-criteria get-neighbors]
  (let [best-frontier-choice (best-choice frontier)]
    (if (stopping-criteria (:node best-frontier-choice))
      (:node best-frontier-choice)
      (recur
        (new-frontier frontier best-frontier-choice get-neighbors heuristic cost)
        heuristic
        cost
        stopping-criteria
        get-neighbors))))

(defn best-first [start-node heuristic cost stopping-criteria get-neighbors]
  (best-first-private
    (conj empty-frontier {:node start-node
                          :cost (cost start-node)
                          :heuristic (heuristic start-node)})
    heuristic
    cost
    stopping-criteria
    get-neighbors))


(defn consume [answer queue heuristic cost stopping-criteria get-neighbors]
  (let [best-element (.take queue)]
    (if (and best-element (stopping-criteria (:node best-element)))
      (compare-and-set! answer nil best-element)
      (do
        (if best-element
          (let [newstuff (frontier-additions cost heuristic get-neighbors best-element)]
            (.addAll queue newstuff)))
        (if (not @answer)
          (recur answer queue heuristic cost stopping-criteria get-neighbors))))))

(defn sleep-on
  ([the-atom time-limit] (sleep-on the-atom time-limit 0))
  ([the-atom time-limit total-elapsed]
  (cond
    @the-atom
    @the-atom
    (> total-elapsed time-limit)
    {:fail :fail}
    :else
    (do (Thread/sleep 10)
        (recur the-atom time-limit (+ total-elapsed 10))))))

(defn parallel-best-first [start-node heuristic cost stopping-criteria get-neighbors]
  (let [answer (atom nil)
        queue (java.util.concurrent.PriorityBlockingQueue. 2222 compare-states)
        added-initial (.offer queue {:node start-node
                                     :cost (cost start-node)
                                     :heuristic (heuristic start-node)})
        num-threads 2
        requests (repeat num-threads (fn [] (consume answer queue heuristic cost stopping-criteria get-neighbors)))
        pool (Executors/newFixedThreadPool num-threads)]
    (doseq [request requests]
      (.submit pool request))
    (:node (sleep-on answer 10000))))
