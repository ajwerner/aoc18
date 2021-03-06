(ns aoc18.core
  (:require
   [aoc18.day1 :as day1]
   [aoc18.day2 :as day2]
   [aoc18.day3 :as day3]
   [aoc18.day4 :as day4]
   [aoc18.day5 :as day5]
   [aoc18.day6 :as day6]
   [aoc18.inputs :as inputs])
  (:gen-class))

(defn -main
  [& args]
  (println "1.1) " (day1/chronal-calibration inputs/day1))
  (println "1.2) " (day1/chronal-calibration2 inputs/day1))
  (println "2.1) " (day2/checksum inputs/day2))
  (println "2.2) " (day2/find-close inputs/day2))
  (println "3.1) " (day3/overlapping-claims inputs/day3))
  (println "3.2) " (day3/unoverlapping-claim inputs/day3))
  (println "4.1) " (day4/best-spot inputs/day4))
  (println "4.2) " (day4/minute-most-asleep inputs/day4))
  (println "5.1) " (day5/reacted-len inputs/day5))
  (println "5.2) " (day5/shortest-with-elimination inputs/day5))
  (println "6.1) " (day6/find-largest-area inputs/day6))
  (println "6.2) " (day6/count-within-bounded-distance inputs/day6 10000)))
