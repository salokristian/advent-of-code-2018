(ns advent-of-code.days.day-2
  (:require [clojure.java.io :as io]))

(def box-IDs
  (clojure.string/split-lines (slurp (io/resource "data_day_2.txt"))))


;;; PART 1
(defn get-total-exact-char-count
  [exact-count]
  (->> box-IDs
       (map frequencies)
       (map vals)
       (keep #(some #{exact-count} %))
       count))

(defn part-one
  []
  (*
    (get-total-exact-char-count 2)
    (get-total-exact-char-count 3)))


;;; PART 2
(defn remove-char
  [pos string]
  (let [before (subs string 0 pos)
        after (subs string (inc pos))]
    (str before after)))

(defn get-duplicate-or-conj
  [seen, new]
  (if (some #{new} seen)
    (reduced new)
    (conj seen new)))

(defn find-first-duplicate
  [coll]
  (let [result (reduce get-duplicate-or-conj #{} coll)]
    (if (coll? result) nil result)))

(defn find-duplicate-without-char-at-pos
  [pos]
  (->> box-IDs
       (map (partial remove-char pos))
       (find-first-duplicate)))

(defn part-two
  []
  (some find-duplicate-without-char-at-pos (range)))
