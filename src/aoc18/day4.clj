(ns aoc18.day4
  (:require [better-cond.core :as b]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(def header-pattern
  #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}")

(defn add-interval [intervals id start end]
  (if (nil? start) 
    intervals
    (update intervals id conj [start end])))

(defn minutes-in-interval [[start end]]
  (t/in-minutes (t/interval start end)))

(def header-format
  (f/formatter "yyyy-MM-dd HH:mm"))

(defn parse-time [record]
  (if-let [t (re-find header-pattern record)]
    (f/parse header-format t)
    (throw (Exception. "fuck "))))

(defn parse-record [record]
  (assoc 
   (b/cond
     :let [begin-matches (re-find #"Guard #([0-9]+) begins shift" record)]
     (some? begin-matches) 
     {:kind :change :guard  (Integer/parseInt (nth begin-matches 1))}
     (re-find #"falls asleep" record) 
     {:kind :asleep}
     (re-find #"wakes up" record) 
     {:kind :awake}
     :else (throw (Exception. (str "bad record " record))))
   :time
   (parse-time record)))

(defn parse-records [records]  
  (map parse-record records))

(defn sort-entries [entries]
  (sort #(t/before? (:time %1) (:time %2)) entries))

(defn group-entries [entries]
  (loop [guard nil 
         asleep-at nil 
         r (first entries)
         entries (rest entries)
         intervals {} ;; maps from guard ID to time intervals asleep
         ]
    (if (nil? r)
      intervals
      (case (:kind r)
        :change (recur (:guard r) nil (first entries) (rest entries) (add-interval intervals guard asleep-at (:time r)))
        :asleep (recur guard (if (nil? asleep-at) (:time r) asleep-at) (first entries)  (rest entries) intervals)
        :awake (recur guard
                      nil
                      (first entries)
                      (rest entries)
                      (add-interval intervals guard asleep-at (:time r)))))))

(defn to-sum [grouped]
  (reduce-kv 
   (fn [m k v] (assoc m k  (reduce +  (map minutes-in-interval v))))
   {} 
   grouped))

(defn max-by-val [m]
  (let [max-sum (apply max (vals m))]
    (->> (seq m)
         (filter (fn [[_ sum]] (= sum max-sum) ))
         (first)
         (first))))

(defn interval-times [[start end :as interval]]
  (for [m (range 0 (minutes-in-interval interval))]
    (let [time (t/plus start (t/minutes m))]
      [(t/hour time) (t/minute time)])))

(defn best-spot [records]
  (let [grouped (->  records 
                    (parse-records)
                    (sort-entries)
                    (group-entries))
        sum-per-guard (to-sum grouped)
        guard-with-max (max-by-val sum-per-guard)
        minute-keys (apply concat (map interval-times (get grouped guard-with-max)))
        minute-counts (reduce 
                       (fn [m v] 
                         (update m v #(if (some? %) (inc %) 1))) 
                       {}
                       minute-keys)
        best-time (max-by-val minute-counts)]
    (* guard-with-max (+ (* 24 (first best-time)) (second best-time)))))
