(ns aoc18.day3)

(defn parse-claim [s]
  "parse-claim parses a claim string nto a map"
  (let [[_ id right down width height :as match] 
        (re-find #"(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)]
    (if (nil? match)
      (throw (RuntimeException. (str "Invalid claim " s)))
      {:id (Integer. id)
       :right (Integer. right)
       :down (Integer. down)
       :width (Integer. width)
       :height (Integer. height)})))

(defn claimed-slots [claim]
  "claimed-lots turns a claim descriptor into a set of pairs of [x, y] coordinates"
  (let [xmin (+ 1 (:right claim))
        xmax (+ xmin (:width claim))
        ymin (+ 1 (:down claim))
        ymax (+ ymin (:height claim))]
    (for [x (range xmin xmax)
          y (range ymin ymax)]
      [x y])))

(defn add-claimed-slots-counts [m claim-slots]
  "takes a map of current claimed-slot-counts and adds claim-slots which is a "
  "seq of [x,y] coordinates"
  (reduce
   (fn [claimed coord]
     (update claimed coord 
             #(if (nil? %) 1 (inc %))))
   m
   claim-slots))

(defn overlapping-claims [claims]
  (->> claims
       (map parse-claim)
       (map claimed-slots)
       (reduce add-claimed-slots-counts {})
       (reduce 
        (fn [c [k v]] (if (> v 1) (inc c) c))
        0)))

(defn all-have-one? [claim-slots slot-counts]
  (let [sc (map 
            (fn [c] (get slot-counts c)) 
            claim-slots)
        count-of-cells (reduce + sc)]
    (= (count claim-slots) (reduce + sc))))

(defn make-claims-map [claims-strings]
  "claims map is a map from "
  (->> claims-strings
       (map parse-claim)
       (map (juxt :id claimed-slots))
       (into {})))

(defn unoverlapping-claim [claims-strings]
  (let [claims-map (let [m (make-claims-map claims-strings)]
                     m)
        slot-counts (reduce add-claimed-slots-counts {} (vals claims-map))
        filtered (filter (fn [[k v]]
                           (all-have-one? v slot-counts)) 
                         claims-map)]
    (key (first filtered))))
