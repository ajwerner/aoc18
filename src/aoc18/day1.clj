(ns aoc18.day1)
  
(defn chronal-calibration [deltas]
  (reduce + deltas))

(defn chronal-calibration2 [deltas]
  (loop [deltas (cycle deltas) freq 0 seen #{0}]
    (let [delta (first deltas)
          next  (+ freq delta)]
      (if (seen next) 
        next
        (recur (rest deltas) next (conj seen next))))))
