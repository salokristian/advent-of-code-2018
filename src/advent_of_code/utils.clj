(ns advent-of-code.utils
  (:require [clojure.java.io :as io]))

(defn file-lines->vector
  "Load lines from file into a vector."
  [file]
  (->> (io/resource file)
       io/reader
       line-seq
       vec))

(defn file->string
  "Read all contents of file into a string."
  [file]
  (->> (io/resource file)
       slurp))

(defn is-same-but-capitalized
  "Return true if s1 and s2 are the the same letter and have different capitalization, false otherwise."
  [s1, s2]
  (let [case-diff 32
        char-diff (Math/abs (compare s1 s2))]
    (and
      (= char-diff case-diff)
      (= (clojure.string/lower-case s1) (clojure.string/lower-case s2)))))

(defn distance [loc1 loc2] (Math/abs (- loc1 loc2)))

(def not-nil? (complement nil?))

(defn zip [& colls] (apply map vector colls))

(defn index-of
  "Find index of element e in coll or nil if e doesn't exist."
  [e coll]
  (first (keep-indexed #(if (= e %2) %1) coll)))

(defn positions
  "Find the indices of vals for which pred returns true."
  [pred coll]
  (keep-indexed (fn [idx x] (if (pred x) idx)) coll))

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
  "Update vector values from index to end to those returned by applying f for each element."
  [coll index f]
  (let [keep-vals (subvec coll 0 index)
        update-vals (subvec coll index)]
    (into keep-vals (map f update-vals))))

(defn update-first
  "Update the first value in coll for which pred returns true."
  [coll pred val]
  (let [idx (-> (positions pred coll) first)]
    (assoc coll idx val)))

(defn update-vals [map vals f]
  "Update vals in map to those returned by applying f to old vals."
  (reduce #(update % %2 f) map vals))