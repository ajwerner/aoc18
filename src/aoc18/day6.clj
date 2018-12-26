(ns aoc18.day6  
  (:require [better-cond.core :as b]))

;; first we want to determine the extremes in each dimension.
;; we'll never fill anything in to these

;; then we want to initialize a map that represents (initially sparsely)
;; everything between the extrema. Then we add all the points to the map and the queue
;; then we go through the queue breadth first. For each thing in the queue, 
;; we look at its neighbors
;; if the neighbor is out of bounds, skip
;; if the neighbor is occupied, skip


(defn bounds [points]
  {:xmax (reduce max (map first points))
   :ymax (reduce max (map second points))
   :xmin (reduce min (map first points))
   :ymin (reduce min (map second points))})


(def neighbor-deltas
  [[1 0] [0 1] [-1 0] [0 -1]])

(defn in-bounds [[x y] {:keys [xmin xmax ymin ymax]}]
  (and 
   (>= x xmin)
   (<= x xmax)
   (>= y ymin)
   (<= y ymax)))

(defn neighbors [[[x y] i] bb]
  (->> (map
        (fn [[dx dy]]
          [[(+ x dx) (+ y dy)] i])
        neighbor-deltas)
       (filter 
        (fn [[p _]] 
          (in-bounds p bb)))))

(defn make-next [m to-add]
  (loop [pending {}
         points to-add]
    (if (empty? points)
      (if (empty? pending)
        nil
        (seq pending))
      (let [[[x y :as p] i] (first points)
            pending 
            (b/cond
              (some? (get m p)) pending
              :let [pi (get pending p)]
              (and (some? pi) (not= pi i)) (assoc pending p -1)
              :else (assoc pending p i))]
        (recur pending (rest points))))))

(defn build-grid [points]
  (let [b (bounds points)
        to-add (map-indexed (fn [i p] [p i]) points)]
    (loop [to-add to-add
           m {}]
      (if (empty? to-add) 
        m
        (let [m (merge (into {} to-add) m)
              next (->> to-add
                        (filter (fn [[_ i]] (>= i 0))) ;; remove nil spots
                        (map #(neighbors % b))
                        (apply concat)
                        (make-next m))]
          (recur next m ))))))

(defn find-center 
  [points]
  [(int
    (Math/floor 
     (/ (reduce + (map first points)) 
        (count points))))
   (int 
    (Math/floor
     (/ (reduce + (map second points)) 
        (count points))))])

(defn distance-sum [points [x y :as p]]
  (reduce
   (fn [s [px py]]
     (+ s
        (int (Math/abs (- x px)))
        (int (Math/abs (- y py)))))
   0
   points))

(defn count-within-bounded-distance 
  "breadth first search"
  [points bound]
  (count  
   (loop [cur (list (find-center points))
          good #{}
          bad #{}]
     (if (empty? cur)
       good
       (let [got (filter 
                  #(not (or 
                         (contains? good %)
                         (contains? bad %)))
                         cur)
             got (map 
                  (fn [p]
                    [p (distance-sum points p)])
                  got)
             not-ok (->> got  
                         (filter 
                          (fn [[p d]]
                                   (>= d bound)))
                         (map first))
             bad (apply conj bad not-ok)
             ok (->> got
                     (filter 
                      (fn [[p d]]
                        (< d bound)))
                            (map first))
             good (apply conj good ok)
             next
             (seq 
              (into #{}
                    (apply concat 
                           (map
                            (fn [[x y]]
                              (map
                               (fn [[dx dy]]
                                 [(+ x dx) (+ y dy)])
                               neighbor-deltas))
                            ok))))]
         (recur next good bad))))))

(defn find-largest-area [points]
  (->> points
       (build-grid)
       (reduce
        (fn [m [p i]]  
          (update m i #(if (nil? %) 1 (inc %)))) 
        {})
       (filter (fn [[i _]] (>= i 0)))
       (reduce (fn [cur [i this]] (max cur this)) 0)))

