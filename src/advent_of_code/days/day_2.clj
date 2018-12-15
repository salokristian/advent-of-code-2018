(ns advent-of-code.days.day-2)

(defn read-lines
  []
  (clojure.string/split-lines (slurp "./resources/data_day_2.txt")))

(defn get-letter-counts
  [boxID]
  (reduce
    #(if (contains? %1 %2)
       (update %1 %2 inc)
       (assoc %1 %2 1))
    {}
    (clojure.string/split boxID #"\B")))

(defn find-occurrences
  [boxID]
  (map #(hash-map (string 1))
   (distinct
     (filter #(or (= 2 %) (= 3 %))
       (vals
         (get-letter-counts boxID))))))

(defn calculate-checksum
  [box-IDs]
  (reduce find-occurrences {2 0 3 0} (read-lines)))

