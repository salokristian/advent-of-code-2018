(ns advent-of-code.days.day-3
  (:require [clojure.java.io :as io]))

(defn string->area-claim
  [string]
  (zipmap
    [:id :x :y :width :height]
    (map read-string (rest (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string)))))

(def fabric-area-claims
  (with-open [reader (io/reader (io/resource "data_day_3.txt"))]
    (doall (map string->area-claim (line-seq reader)))))


;;; PART 1
(defn partition-by-overlap
  [coll start len]
  (let [end (+ start len)]
    [(subvec coll 0 start) (subvec coll start end) (subvec coll end)]))

(defn update-map-dimension
  [start len process-overlapping dimension]
  (let [[before, overlapping, after]
        (partition-by-overlap dimension start len)]
    (vec (concat before
             (vec (map process-overlapping overlapping))
             after))))

(defn area-claim->location-in-map
  [fabric-map, area-claim]
  (let [update-elem #(conj % (get area-claim :id))
        update-row (partial update-map-dimension (get area-claim :x) (get area-claim :width) update-elem)]
    (update-map-dimension (get area-claim :y) (get area-claim :height) update-row fabric-map)))

(defn vec2d
  [x y val]
  (vec (repeat y (vec (repeat x val)))))

(defn flatten-one-level
  [coll]
  (apply concat coll))

(defn part-one
  []
  (->> fabric-area-claims
       (reduce area-claim->location-in-map (vec2d 1000 1000 []))
       (flatten-one-level)
       (filter #(> (count %) 1))
       (count)))


;;; PART 2
(defn get-overlapping-claims
  []
  (->> fabric-area-claims
       (reduce area-claim->location-in-map (vec2d 1000 1000 []))
       (flatten-one-level)
       (filter #(> (count %) 1))
       (flatten-one-level)
       (set)))

(defn get-all-claims
  []
  (set (map #(get % :id) fabric-area-claims)))

(defn part-two
  []
  (first (clojure.set/difference (get-all-claims) (get-overlapping-claims))))