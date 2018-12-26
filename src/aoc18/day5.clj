(ns aoc18.day5
  (:require
   [clojure.string :refer [upper-case]]))

(defn react [p]
  (loop [head (list) cur p]
    (let [[v1 v2] (take 2 cur)] 
      (if (nil? v2)
        (concat (reverse head) cur)
        (let  [upper-1 (Character/isUpperCase v1)
               upper-2 (Character/isUpperCase v2)
               match (and (not= upper-1 upper-2)
                          (= (upper-case v1) (upper-case v2)))]
          (if match
            (if (> (count head) 0)
              (recur (rest head) (conj (drop 2 cur) (first head)))
              (recur head (drop 2 cur)))
            (recur (conj head v1) (conj (rest cur)))))))))

(defn filter-unit [lower-case]
  (let [uc (Character/toUpperCase lower-case)]
    (fn [c]
      (not (or (= c uc) (= c lower-case))))))

(defn react-polymer [polymer]
  (apply str (react (seq polymer))))

(defn reacted-len [polymer]
  (count (react (seq polymer))))

(def letters 
  (seq "abcdefghijklmnopqrstuvwxyz"))

(defn shortest-with-elimination [polymer]
  (let [s (seq polymer)]
    (reduce 
     (fn [min-so-far to-eliminate]
       (let [with-removed (react (filter (filter-unit to-eliminate) s))
             n (count with-removed)]
         (if (<  n min-so-far) n min-so-far)))
     (count polymer)
     letters)))

