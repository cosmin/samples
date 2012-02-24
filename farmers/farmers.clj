(in-ns 'user)

;;; for convenience, code from clojure.contrib.combinatorics

(defn- index-combinations
  [n cnt]
  (lazy-seq
   (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
         iter-comb
         (fn iter-comb [c j]
           (if (> j n) nil
               (let [c (assoc c j (dec (c j)))]
                 (if (< (c j) j) [c (inc j)]
                     (loop [c c, j j]
                       (if (= j 1) [c j]
                           (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
         step
         (fn step [c j]
           (cons (rseq (subvec c 1 (inc n)))
                 (lazy-seq (let [next-step (iter-comb c j)]
                             (when next-step (step (next-step 0) (next-step 1)))))))]
          (step c 1))))

(defn combinations
  "All the unique ways of taking n different elements from items"
  [items n]
  (let [v-items (vec (reverse items))]
    (if (zero? n) (list ())
        (let [cnt (count items)]
          (cond (> n cnt) nil
                (= n cnt) (list (seq items))
                :else
                (map #(map v-items %) (index-combinations n cnt)))))))

(defn subsets
  "All the subsets of items"
  [items]
  (mapcat (fn [n] (combinations items n))
            (range (inc (count items)))))

;;; code to solve the problem

(defn sum [coll]
  (reduce + coll))

(defn all-weight-possibilities [stones]
  (let [stone-set (set stones)]
    (for [left (subsets stones)
          right (subsets (apply disj stone-set left))]
      (list left right))))

(defn can-weigh? [weight stones]
  (first (filter #(not (nil? %))
                 (for [[left, right] (all-weight-possibilities stones)]
                   (if (= (sum left) (+ weight (sum right)))
                     [left right]
                     nil)))))

(defn possible-stones []
  (filter #(= 40 (sum %1)) (combinations (range 1 40) 4)))

(defn solution? [stones]
  (every? #(not (nil? %1))
          (map #(can-weigh? %1 stones) (range 1 41))))

(defn solve-farmer-problem []
  (first (filter solution? (possible-stones))))

(println (solve-farmer-problem))
