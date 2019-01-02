(ns advent-of-code.days.day-6
  (:require [advent-of-code.utils :as utils])
  (:require [clojure.string :as str]))

(defn parse-coordinates [idx coord-str]
  (->> (str/split coord-str #", ")
       (map #(Integer/parseInt %))
       (zipmap [:x :y])
       (conj {:id idx})))

(def coordinates
  (->> (utils/file-lines->vector "data_day_6.txt")
       (map-indexed parse-coordinates)
       vec))

;;;; PART 1

(defn manhattan-distance [coord1 coord2]
  (+
    (utils/distance (:x coord1) (:x coord2))
    (utils/distance (:y coord1) (:y coord2))))

(defn calculate-closest-coordinate [coordinates location]
  (let [coords-with-distance (map #(assoc % :distance (manhattan-distance location %)) coordinates)
        closest-coord (apply min-key :distance coords-with-distance)
        closest-occurrences (filter (comp #{(:distance closest-coord)} :distance) coords-with-distance)]
    (if (= 1 (count closest-occurrences)) (:id closest-coord) -1 )))

(defn get-coordinate-area [coordinates]
  (let [x-coords (map :x coordinates)
        y-coords (map :y coordinates)]
    {:xmin (apply min x-coords)
     :xmax (apply max x-coords)
     :ymin (apply min y-coords)
     :ymax (apply max y-coords)}))

(defn get-locations-with-closest-coord [coordinates area]
  (let [locations (for [x (range (:xmin area) (inc (:xmax area)))
                        y (range (:ymin area) (inc (:ymax area)))]
                    {:x x :y y})]
    (map #(assoc % :closest-coord (calculate-closest-coordinate coordinates %)) locations)))

(defn overlapping? [location] (= -1 (:closest-coord location)))

(defn on-areas-edge? [location area]
  (or
    (some #{(:x location)} [(:xmin area) (:xmax area)])
    (some #{(:y location)} [(:ymin area) (:ymax area)])))

(defn remove-invalid-locations [locations area]
  (let [invalid-coords (->> (filter #(or (on-areas-edge? % area) (overlapping? %)) locations)
                             (map :closest-coord)
                             set)]
    (remove #(contains? invalid-coords (:closest-coord %)) locations)))

(defn part-one []
  (let [area (get-coordinate-area coordinates)
        locs-with-closest (get-locations-with-closest-coord coordinates area)
        valid-locs (remove-invalid-locations locs-with-closest area)]
    (->> (map :closest-coord valid-locs)
         frequencies
         vals
         (apply max))))