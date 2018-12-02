(ns aoc18.day2)

(defn checksum [ids]
  (let [inc-count 
        (fn [c]
          (if (nil? c) 1 (+ c 1)))
        inc-letter-count 
        (fn [counts l]
          (update counts l inc-count))
        letter-counts 
        (fn [id] 
          (reduce inc-letter-count {} (seq id)))
        num-letters-with-count
        (fn [c counts]
          (count 
           (reduce-kv (fn [m k v]
                        (if (= v c) m (dissoc m k)))
                      counts counts)))
        num-with-count
        (fn [c counts-list]
          (->> counts-list
               (map (partial num-letters-with-count c))
               (reduce #(if (= 0 %2) %1 (inc %1)) 0)))
        counts-list (map letter-counts ids)
        with-2 (num-with-count 2 counts-list)
        with-3 (num-with-count 3 counts-list)]
    (* with-2 with-3)))

(defn find-close [ids]
  ;; we want to, for each id, remove letter at column c and store the column and change as a key
  ;; and then we want to map it to a value. we first want to check if the column and store has something
  ;; if it does, then we're done
  (loop [id nil ids ids i 0 m {}]
    (cond
      (and (nil? id) (nil? ids)) 
      nil
      (= i (count id)) 
      (recur (first ids) (rest ids) 0 m)
      :else 
      (let [removed (str (subs id 0 i) (subs id (+ i 1)))
            k [removed i]
            exists (some? (m k))]
        (if exists
          removed
          (recur id ids (+ i 1) (assoc m k id)))))))
