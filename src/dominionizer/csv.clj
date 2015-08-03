(ns dominionizer.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def csv
  (drop 2 (csv/read-csv (slurp (io/resource "cards.csv")))))

(def header
  [:name :expansion :action :attack :curse :duration :reaction :treasure :victory :coin-cost :potion-cost :action :buy :card :coin :vp :text])

(def types
  #{:action :attack :curse :duration :reaction :treasure :victory})

(defn transform
  [row]
  (zipmap header (map (fn [v] (case v
                               "1" true
                               "" nil
                               v)) row)))

(defn derive-types
  [card]
  (-> (apply dissoc card types)
      (assoc :types (set (filter card types)))))

(defn cost
  [coins potions]
  (let [coins (or (and coins (Integer/parseInt (second (re-matches #"\A\$(\d+)\z" coins)))) 0)
        potions (or (and potions (Integer/parseInt (second (re-matches #"\A(\d+)p\z" potions)))) 0)]
    [coins potions]))

(defn derive-cost
  [card]
  (let [{:keys [coin-cost potion-cost]} card]
    (-> card
        (dissoc :coin-cost :potion-cost)
        (assoc :cost (cost coin-cost potion-cost)))))

(defn discard-effects
  [card]
  (dissoc card :action :vp :buy :coin :card))

(def cards
  (mapv (comp discard-effects derive-cost derive-types transform) csv))

(def by-expansion
  (reduce (fn [accum card]
            (update accum (:expansion card) (fnil conj []) (dissoc card :expansion)))
          {}
          cards))

(defn transform!
  []
  (spit "resources/cards.edn" (with-out-str (pprint by-expansion))))

(defn -main
  [& args])
