(ns advent-of-code.days.day-7
  (:require [advent-of-code.utils :as utils]
            [advent_of_code.kahn.kahn :as kahn]))

(defn parse-instruction [instruction]
  {:step (get instruction 36)
   :do-before (get instruction 5)})

(defn add-instruction-to-graph
  [graph instruction]
  (if (contains? graph (:do-before instruction))
    (update graph (:do-before instruction) #(conj % (:step instruction)))
    (assoc graph (:do-before instruction) #{(:step instruction)})))

(defn instructions->dep-graph [filename]
  (->> (utils/file-lines->vector filename)
       (map parse-instruction)
       (reduce add-instruction-to-graph {})))

(defn lexical-comparator [a b]
  (compare a b))

(defn part-one []
  (-> (instructions->dep-graph "data_day_7.txt")
      (kahn/kahn-sort lexical-comparator)))
