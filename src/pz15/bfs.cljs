(ns pz15.bfs
  (:require
   [tailrecursion.priority-map :as pm]
   [pz15.db :as db]
   ))

(defonce ^:const UP 1)

(defonce ^:const DOWN -1)

(defonce ^:const LEFT 10)

(defonce ^:const RIGHT -10)

(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))

(defn with-index [[x y :as v]]
  (map-values (fn [a] [x a]) y))

(defn sum-seq [s]
  (reduce + s))

(defn transpose-coords [coords]
  (map (fn [[n [x y]]]
         [n [y x]]) coords))

(defn num-to-row [n]
  (if (= n 0)
    0
    (+ (quot (- n 1) 4) 1)))

(defn row-to-freqs [row]
  (frequencies (mapv num-to-row (keys row))))

(defn state-to-rows [{:keys [board zero]}]
  (let [new-board (mapv row-to-freqs board)]
    {:board new-board :zero zero}))

(defn state-coords [s]
  (into {} (mapv with-index (mapv
                             (fn [x] x)
                             (map-indexed vector (:board s))))))

(defn transpose-state [s]
  (let [coords (state-coords s)
        new-zero (nth (get coords 0) 1)
        new-board (let [tc (into (sorted-map) (seq
                                               (group-by (fn [[n [x y]]] x)
                                                         (transpose-coords
                                                          coords))))]

                    (for [[k v] tc]
                      (into {} (mapv (fn [[x [a b]]] {x b}) v))))]

    {:board new-board :zero new-zero}))

(defn num-to-col [n]
  (if (= n 0)
    0
    (if (= (rem n 4) 0) 4 (rem n 4))))

(defn col-to-freqs [row]
  (frequencies (mapv num-to-col (keys row))))

(defn state-to-cols [{:keys [board zero]}]
  (let [new-board (mapv col-to-freqs board)]
    {:board new-board :zero zero}))

(defonce target-state
  {:board
   (mapv (fn [x] (zipmap x (range 4)))
         (mapv vec (partition 4 (map (fn [x] (rem (+ x 1) 16)) (range 16))))) :zero 3})

(defn hor-dist [s]
  (let [row-state (state-to-rows s)]
    (get db/dists row-state)))

(defn vert-dist [s]
  (let [ts (transpose-state s)
        row-state (state-to-cols ts)]
    (get db/dists row-state)))

(defn walk-dist [s]
  (+ (hor-dist s) (vert-dist s)))

(defn manh-dist [s t]
  (let [c1 (state-coords s)
        c2 (state-coords t)]
    (sum-seq (for [[k v] c1]
               (let [[x1 y1] v
                     [x2 y2] (get c2 k)]
                 (+ (abs (- x1 x2)) (abs (- y1 y2))))))))

(defn inside? [x] (and (>= x 0) (< x 4)))

(defn sort-row-by-value [row]
  (into {} (sort-by (fn [a] (nth a 1)) (vec row))))

(defn switch-hor [board zero pos]
  (let [zero-row (nth board zero)
        zero-pos (get zero-row 0)
        a (vec zero-row)
        x (nth a pos)
        y (nth a zero-pos)
        [n1 i1] x
        [n2 i2] y]
    (assoc board zero (into {} (assoc (assoc a i1 [n2 i1]) i2 [n1 i2])))))

(defn switch-vert [board zero pos]
  (let [vv (mapv vec board)
        zero-pos (get (nth board zero) 0)
        zero-row (nth vv zero)
        row (nth vv pos)
        [n1 i1] (nth zero-row zero-pos)
        [n2 i2] (nth row zero-pos)
        new-zero-row (into {} (assoc zero-row i1 [n2 i2]))
        new-row (into {} (assoc row i1 [n1 i1]))]
    (assoc (assoc board zero new-zero-row) pos new-row)))

(defn next-node-for-action [{:keys [state length] :as n} action]
  (let [board (:board state)
        zero (:zero state)
        zero-row (nth board zero)
        zero-pos (get zero-row 0)]
    (cond
      (= action UP) (if (inside? (+ zero 1)) [{:prev n :length (+ length 1) :state {:board (switch-vert board zero (+ zero 1)) :zero (+ zero 1)}}] [])
      (= action DOWN) (if (inside? (- zero 1)) [{:prev n :length (+ length 1) :state {:board (switch-vert board zero (- zero 1)) :zero (- zero 1)}}] [])
      (= action LEFT) (if (inside? (+ zero-pos 1)) [{:prev n :length (+ length 1) :state {:board (switch-hor board zero (+ zero-pos 1)) :zero zero}}] [])
      :else (if (inside? (- zero-pos 1)) [{:prev n :length (+ length 1) :state {:board (switch-hor board zero (- zero-pos 1)) :zero zero}}] []))))

(defn expand-with-dists [node reached q]
  (let [next-nodes (flatten (for [action [UP DOWN LEFT RIGHT]]
                              (next-node-for-action node action)))]
    (loop [nn next-nodes p q r reached]
      (if (= (count nn) 0)
        {:queue p :reached r}
        (let [[n & ns] nn
              s (:state n)
              new-prio (+ (:length n) (walk-dist s) (/ (* 2 (manh-dist s target-state)) 3))
              new-vals (if (not (contains? r s))
                         {:reached (assoc r s n) :queue (assoc p n new-prio)}
                         {:reached r :queue p})
              new-q (:queue new-vals)
              new-r (:reached new-vals)]
          (recur ns new-q new-r))))))

(defn node-to-list [node]
  (loop [n node acc []]
    (if (not (some? (:prev n)))
      (conj acc (:state n))
      (recur (:prev n) (conj acc (:state n))))))


;(defn action-to-event [a]
;  (cond 
;    (= a UP) [::events/key-up]
;    (= a DOWN) [::events/key-down]
;    (= a LEFT) [::events/key-left]
;    (= a RIGHT) [::events/key-right]
;    )
;  )


(defn bfs [start-state]
  (let [start-node {:state start-state :length 0}
        q (pm/priority-map start-node 0)
        reached {start-state start-node}]
    (loop [r reached p q]
      (if (and (not (empty? p)) (not (= target-state (:state (nth (peek p) 0)))))
        (let [n (nth (peek p) 0)
              next-result (expand-with-dists n r (pop p))
              new-reached (:reached next-result)
              new-queue (:queue next-result)]
          (recur new-reached new-queue))
        (node-to-list (nth (peek p) 0))))))
