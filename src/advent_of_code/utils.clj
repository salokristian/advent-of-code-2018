(ns advent-of-code.utils
  (:require [clojure.java.io :as io]))

(defn file-lines->vector
  "Load lines from file into a vector."
  [file]
  (->> (io/resource file)
       io/reader
       line-seq
       vec))

(defn index-of
  "Find index of element e in coll or nil if e doesn't exist."
  [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn get-index-of-max-val
  "Find index of max value in coll."
  [coll]
  (index-of (apply max coll) coll))

(defn get-max-entry
  "Find max entry in map as determined by calling f for each entry in map.
  f should return an integer."
  [map f]
  (apply max-key f map))

(defn update-from
  "Update vector values from index to end to those acquired from calling f for each element."
  [coll index f]
  (let [keep-vals (subvec coll 0 index)
        update-vals (subvec coll index)]
    (into keep-vals (map f update-vals))))