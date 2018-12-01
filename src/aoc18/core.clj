(ns aoc18.core
  (:require [aoc18.day1 :as day1]
            [aoc18.day1-inputs :as day1-inputs])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "1.1) " (day1/chronal-calibration day1-inputs/input))
  (println "1.2) " (day1/chronal-calibration2 day1-inputs/input)))
