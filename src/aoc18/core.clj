(ns aoc18.core
  (:require
   [aoc18.day1 :as day1]
   [aoc18.day2 :as day2]
   [aoc18.inputs :as inputs])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "1.1) " (day1/chronal-calibration inputs/day1))
  (println "1.2) " (day1/chronal-calibration2 inputs/day1))
  (println "2.1) " (day2/checksum inputs/day2))
  (println "2.2) " (day2/find-close inputs/day2)))
