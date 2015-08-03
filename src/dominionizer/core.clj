(ns dominionizer.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn choose
  [n s]
  (when (> n (count s))
    (throw (IllegalArgumentException. "Insufficient choices")))
  (set (take n (shuffle s))))

(def expansions
  (edn/read-string (slurp (io/resource "cards.edn"))))

(defn sample-from-expansion
  [expansion n]
  (let [candidates (->> (get expansions expansion)
                        (remove :core)
                        (remove #(= false (:supply %))))]
    (choose n candidates)))

(def alchemy-minimum 3)

(defn minimum-for-expansion
  [minimums expansion]
  (get minimums expansion (:default minimums)))

(defn sample-from-expansions
  [total minimums expansions]
  (let [minimums (->> expansions
                      (map (juxt identity (partial minimum-for-expansion minimums)))
                      (into {}))
        expansions (into [] expansions)
        totals (loop [totals minimums]
                 (let [current (reduce + (vals totals))]
                   (cond (< total current)
                         (throw (IllegalArgumentException. "Minimums cannot be satisfied"))
                         (> total current)
                         (recur (update totals (rand-nth expansions) inc))
                         :else
                         totals)))]
    (into {} (map (juxt first (partial apply sample-from-expansion)) totals))))

(defn sample-cores
  [cores samples]
  (let [cores (cond-> cores (samples "Alchemy") (conj :alchemy))]
    (reduce-kv (fn [accum expansion cards]
                 (assoc accum expansion (set (filter (comp cores :core) cards))))
               {}
               expansions)))

(def standard-rules
  {:total 10
   :minimums {:alchemy 3
              :default 0}
   :expansions (disj (set (keys expansions)) "Promo")
   :expansion-count 2
   :cores #{true}})

(defn randomize
  [rules]
  (let [{:keys [total minimums cores expansions expansion-count]} rules
        expansions (choose expansion-count expansions)
        samples (sample-from-expansions total minimums expansions)
        cores (sample-cores cores samples)]
    (merge-with into cores samples)))
