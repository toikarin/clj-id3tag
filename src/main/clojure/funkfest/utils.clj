(ns funkfest.utils)

(defn take-min
  [n coll]
  (let [coll-count (count coll)]
    (cond
      (= coll-count n) coll
      (> coll-count n) (take n coll)
      (< coll-count n) (concat coll (replicate (- n coll-count) nil)))))

(defn split-at-pos
  [pos-coll data]
  (loop [splitted-data (transient [])
         cur-pos-coll pos-coll
         cur-data data]
    (if-not (empty? cur-pos-coll)
      (let [next-pos (first cur-pos-coll)
            num-of-elements (if (= :rest next-pos) (count cur-data) next-pos)
            cur-data-splitted (split-at num-of-elements cur-data)
            next-element (cond 
                           (not (pos? num-of-elements)) nil
                           (= 1 num-of-elements) (ffirst cur-data-splitted)
                           :else (take-min num-of-elements (first cur-data-splitted)))]
        (recur
          (conj! splitted-data next-element)
          (rest cur-pos-coll)
          (fnext cur-data-splitted)))
      (persistent! splitted-data))))

