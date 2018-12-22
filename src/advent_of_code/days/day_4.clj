(ns advent-of-code.days.day-4
  (:require [java-time.core :as jtime-core])
  (:require [java-time.local :as jtime-local])
  (:require [advent-of-code.utils :as utils]))

;;;; Data parsing

(defn parse-date
  [string]
  (->> (rest (re-find #"\[([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\]" string))
       (map #(Integer/parseInt %))
       (apply jtime-local/local-date-time)
       (hash-map :time)))

(defn parse-action
  [string]
  (let [match-start (re-find #"Guard #([0-9]+) begins shift" string)
        match-asleep (re-find #"falls asleep" string)
        match-woke-up (re-find #"wakes up" string)]
    (cond
      match-start (conj {:type :start-shift} {:id (Integer/parseInt (last match-start))})
      match-asleep {:type :fell-asleep}
      match-woke-up {:type :woke-up})))

(defn parse-shift-data
  [shift-string]
  (conj (parse-date shift-string) (parse-action shift-string)))

(def shift-data
  (->> (utils/file-lines->vector "data_day_4.txt")
       (map parse-shift-data)
       (sort-by :time)))

;;;; PART 1

(defn change-current-guard
  [slept-mins guard-id]
  (apply assoc slept-mins :current-guard guard-id (if-not
                                                    (contains? slept-mins guard-id)
                                                    [guard-id (vec (repeat 60 0))])))

(defn shift-data->slept-mins
  [slept-mins shift-data]
  (let [minute (jtime-core/as (:time shift-data) :minute-of-hour)
        current-guard (:current-guard slept-mins)]
    (case (:type shift-data)
      :start-shift (change-current-guard slept-mins (:id shift-data))
      :fell-asleep (update slept-mins current-guard utils/update-from minute inc)
      :woke-up (update slept-mins current-guard utils/update-from minute dec))))

(defn get-slept-mins
  [shift-data]
  (dissoc (reduce shift-data->slept-mins {} shift-data) :current-guard))

(defn part-one []
  (let [slept-mins (get-slept-mins shift-data)
        sleepyhead (key (utils/get-max-entry slept-mins #(reduce + 0 (val %))))
        sleeps-most-likely-at (utils/get-index-of-max-val (get slept-mins sleepyhead))]
    (* sleepyhead sleeps-most-likely-at)))

;;;; PART 2
(defn find-max-sleep-min-and-count
  [slept-mins]
  (let [max-sleep-min (utils/get-index-of-max-val slept-mins)
        slept-at-min-count (get slept-mins max-sleep-min)]
    [max-sleep-min slept-at-min-count]))

(defn part-two
  []
  (let [max-asleep-minutes (->> (get-slept-mins shift-data)
                                (map #(hash-map (key %) (find-max-sleep-min-and-count (val %))))
                                (apply conj))
        most-often-asleep-on-same-minute (key (utils/get-max-entry max-asleep-minutes #(second (val %))))
        max-minute (first (get max-asleep-minutes most-often-asleep-on-same-minute))]
    (* most-often-asleep-on-same-minute max-minute)))
