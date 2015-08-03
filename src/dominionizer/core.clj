(ns dominionizer.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn choose
  [n s]
  (when (> n (count s))
    (throw (IllegalArgumentException. "Insufficient choices")))
  (set (take n (shuffle s))))

(def expansions
  (let [expansions (edn/read-string (slurp (io/resource "cards.edn")))]
    (reduce-kv (fn [accum expansion cards]
                 (->> cards
                      (mapv (fn [card] (assoc card :expansion expansion)))
                      (assoc accum expansion)))
               {}
               expansions)))

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
  (let [chances (reduce-kv (fn [accum expansion cards]
                             (into accum (repeat (count cards) expansion)))
                           []
                           samples)
        big-money? (= "Prosperity" (rand-nth chances))
        shelters? (= "Dark Ages" (rand-nth chances))
        alchemy? (samples "Alchemy")
        cores (cond-> cores
                      big-money?
                      (conj :big-money)
                      shelters?
                      (conj :shelters)
                      alchemy?
                      (conj :alchemy))]
    (reduce-kv (fn [accum expansion cards]
                 (assoc accum expansion (set (filter (comp cores :core) cards))))
               {}
               expansions)))

(defn sample-extras
  [samples]
  (let [young-witch? (some (fn [card] (= "Young Witch" (:name card)))
                           (get samples "Cornucopia"))
        bane (when young-witch?
               (->> (reduce into (vals expansions))
                    (filter (fn [card]
                              (let [[coins potions] (:cost card)]
                                (and (= 0 potions)
                                     (or (= 2 coins) (= 3 coins))))))
                    (remove (reduce into #{} (vals samples)))
                    rand-nth))]
    (when bane
      {(:expansion bane) #{(assoc bane :extras #{:bane})}})))

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
        cores (sample-cores cores samples)
        extras (sample-extras samples)
        all (merge-with into cores samples extras)]
    (reduce-kv (fn [accum expansion cards]
                 (let [names (mapv (fn [card] (select-keys card #{:name :extras}))
                                   (sort-by :name cards))]
                   (cond-> accum
                           (seq names)
                           (assoc expansion names))))
               {}
               all)))
