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
                        (remove #(= false (:supply %)))
                        (remove (comp :event :types)))]
    (choose n candidates)))

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

(defn has-card?
  [samples expansion card-name]
  (some (fn [card] (= card-name (:name card)))
        (get samples expansion)))

(defn sample-young-witch
  [samples]
  (let [young-witch? (has-card? samples "Cornucopia" "Young Witch")
        basis (reduce into (map expansions (keys samples)))
        bane (when young-witch?
               (->> basis
                    (filter (fn [card]
                              (let [[coins potions] (:cost card)]
                                (and (= 0 potions)
                                     (or (= 2 coins) (= 3 coins))))))
                    (remove (reduce into #{} (vals samples)))
                    rand-nth))]
    (when bane
      {(:expansion bane) #{(assoc bane :extras #{:bane})}})))

(defn sample-tournament
  [samples]
  (let [tournament? (has-card? samples "Cornucopia" "Tournament")
        prizes (when tournament?
                 (->> (get expansions "Cornucopia")
                      (filter (comp :prize :types))
                      (mapv (fn [card]
                              (assoc card :extras #{:prize})))))]
    (when prizes
      {"Cornucopia" (set prizes)})))

(defn sample-events
  [samples]
  (let [adventure? (seq (samples "Adventures"))
        basis (reduce into (map expansions (keys samples)))
        events (when adventure?
                 (->> basis
                      shuffle
                      (take 10)
                      (filter (comp :event :types))
                      (take 2)
                      (map (fn [card] (assoc card :extras #{:event})))))]
    (when (seq events)
      {"Adventures" (set events)})))

(defn sample-extras
  [samples]
  (merge-with (fnil into #{})
              (sample-young-witch samples)
              (sample-tournament samples)
              (sample-events samples)))

(def standard-rules
  {:total 10
   :minimums {"Alchemy" 3
              :default 0}
   :expansions (disj (set (keys expansions)) "Promo")
   :expansion-counts #{2 3}
   :cores #{true}})

(defn randomize
  [rules]
  (let [{:keys [total minimums cores expansions expansion-counts]} rules
        expansion-count (rand-nth (seq expansion-counts))
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
