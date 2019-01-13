(ns advent-of-code.days.day-7
  (:require [advent-of-code.utils :as utils]
            [advent_of_code.kahn.kahn :as kahn]
            [clojure.set :as set]))

;;;; PART 1

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
      (kahn/kahn-sort lexical-comparator)
      (clojure.string/join)))

;;;; PART 2

(defn get-worker
  ([] (get-worker nil 0))
  ([work-on work-left]
   {:work-on work-on :work-left work-left}))

(def workers (-> (repeat 5 (get-worker)) vec))

(defn add-to-next-steps [graph next-steps add-steps]
  (let [valid-add-steps (set/intersection (kahn/no-incoming graph) add-steps)]
    (set/union next-steps valid-add-steps)))

(defn decrease-work-left [decrease-by worker]
  (update worker :work-left #(- % decrease-by)))

(defn finish-readiest-worker [workers]
  (let [readiest-worker (->> (filter #(utils/not-nil? (:work-on %)) workers)
                             (apply min-key #(:work-left %)))
        workers' (->> (replace {readiest-worker (get-worker)} workers)
                      (mapv #(decrease-work-left (:work-left readiest-worker) %)))]
    [readiest-worker workers']))

(defn step->seconds [step] (+ (compare step \A) 61))

(defn start-working-on [workers steps]
  (reduce (fn [workers step]
            (utils/update-first workers #(nil? (:work-on %)) (get-worker step (step->seconds step))))
          workers
          steps))

(defn process-next-finished-step
  [graph workers time-taken next-steps]
  (let [[finished-worker workers'] (finish-readiest-worker workers)
        time-taken' (+ time-taken (:work-left finished-worker))
        reachable-steps (graph (:work-on finished-worker))
        graph' (reduce #(update-in % [(:work-on finished-worker)] kahn/without %2) graph reachable-steps)
        next-steps' (add-to-next-steps graph' next-steps reachable-steps)]
    [graph' workers' time-taken' next-steps']))

(defn finished? [graph next-steps]
  (and
    (every? empty? (vals graph))
    (empty? next-steps)))

(defn startable-step-count [next-steps workers]
  (let [free-workers (filter #(nil? (:work-on %)) workers)
        step-count (count (utils/zip free-workers next-steps))]
    (if (pos? step-count) step-count)))

(defn take-n [steps cmp n]
  (let [take-steps (take n (apply sorted-set-by cmp steps))]
    [take-steps (set/difference steps (set take-steps))]))

(defn kahn-sort-workers
  ([graph workers]
   (kahn-sort-workers (kahn/normalize graph) workers 0 (kahn/no-incoming graph)))
  ([graph workers time-taken next-steps]
   (if (finished? graph next-steps)
     time-taken
     (if-let [start-n-steps (startable-step-count next-steps workers)]
       (let [[work-on-steps next-steps'] (take-n next-steps lexical-comparator start-n-steps)
             workers' (start-working-on workers work-on-steps)]
         (apply kahn-sort-workers (process-next-finished-step graph workers' time-taken next-steps')))
       (apply kahn-sort-workers (process-next-finished-step graph workers time-taken next-steps))))))

(defn part-two []
  (-> (instructions->dep-graph "data_day_7.txt")
      (kahn-sort-workers workers)))