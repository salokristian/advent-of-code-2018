(ns advent-of-code.day-1
  (:require [clojure.java.io :as io]))

(defn read-numbers
  []
  (with-open [reader (io/reader "./resources/data_day_1.txt")]
    (vec (map read-string (line-seq reader)))))

(defn get-resulting-freq
  [freqs]
  (reduce + freqs))

(defn calibrate-device [freqs]
  (loop [xs (cycle freqs)
         seen-freqs #{}
         total-freq 0]
    (if (contains? seen-freqs total-freq)
      total-freq
      (recur
        (rest xs)
        (conj seen-freqs total-freq)
        (+ total-freq (first xs))))))

(defn part-one
  []
  (get-resulting-freq (read-numbers)))

(defn part-two
  []
  (calibrate-device (read-numbers)))