(ns advent-of-code.days.day-5
  (:require [advent-of-code.utils :as utils])
  (:require [clojure.string :as string]))

(def polymer (utils/file->string "data_day_5.txt"))

;;;; PART 1

(defn add-to-polymer
  [polymer unit]
  (let [adjacent-unit (last polymer)]
    (if (utils/is-same-but-capitalized unit adjacent-unit)
      (butlast polymer)
      (concat polymer (str unit)))))

(defn part-one
  []
  (count (reduce add-to-polymer "" polymer)))

;;;; PART 2

(defn update-polymer-wo-unit
  [unit polymer-wo-unit]
  (let [wo-unit (first polymer-wo-unit)
        polymer (second polymer-wo-unit)]
    (if (= (string/lower-case unit) wo-unit)
      polymer-wo-unit
      [wo-unit (add-to-polymer polymer unit)])))

(defn add-to-polymers-wo-unit
  [polymers-wo-unit unit]
  (mapv (partial update-polymer-wo-unit unit) polymers-wo-unit))

(defn part-two
  []
  (let [all-units (distinct (map string/lower-case polymer))
        unit-polymer (map vector all-units (repeat (count all-units) '()))
        polymers-wo-unit (reduce add-to-polymers-wo-unit unit-polymer polymer)]
    (->> polymers-wo-unit
         (map #(count (second %)))
         (apply min))))