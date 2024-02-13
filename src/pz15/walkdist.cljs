(ns pz15.walkdist
  (:require 
   [tailrecursion.priority-map :as pm]
    [clojure.set :refer [map-invert]])
)

(def ^:const UP 1)

(def ^:const DOWN -1)


(defn inside? [x] (and (>= x 0) (< x 4)))


(def target-state
  {:board
   (mapv (fn [x] (zipmap x (range 4)))
         (mapv vec (partition 4 (map (fn [x] (rem (+ x 1) 16)) (range 16))))) :zero 3})

(defn inc-map [m k]
  (let [v (get m k 0)]
    (if (> v 1)
      (assoc (assoc m k (- v 1)) 0 1)
      (assoc (dissoc m k) 0 1)
      )
    )
  )

(defn change-zero-row [m k]
  (let [v (get m k 0)]
    (if (> v 0)
      (assoc (dissoc m 0) k (+ v 1))
      (assoc (dissoc m 0) k 1)
      )
    )
  )

(defn map-values [f m]
  (->> (map (fn [[k v]] [k (f v)]) m)
       (into {})))

(defn with-index [[x y :as v]]
  (map-values (fn [a] [x a]) y)
  )

(defn state-coords [s]
  (into {} (mapv with-index (mapv
                              (fn [x] x)
                              (map-indexed vector (:board s)))))
  )

(defn transpose-coords [coords]
  (map (fn [[n [x y]]]
         [n [y x]]
         ) coords
       )
  )

(defn transpose-state [s]
  (let [coords (state-coords s)
        new-zero (nth (get coords 0) 1)
        new-board (let [tc (into (sorted-map) (seq
                                                (group-by (fn [[n [x y]]] x)
                                                          (transpose-coords
                                                            coords)
                                                          )
                                                )
                                 )]
                    (for [[k v] tc]
                      (into {} (mapv (fn [[x [a b]]] {x b}) v))
                      )
                    )
        ]
    {:board new-board :zero new-zero}
    )
  )


(defn sum-seq [s]
  (reduce + s)
  )

(defn num-to-row [n]
  (if (= n 0)
    0
    (+ (quot (- n 1) 4) 1)
    )
  )

(defn num-to-col [n]
  (if (= n 0)
    0
    (if (= (rem n 4) 0) 4 (rem n 4))
    )
  )

(defn row-to-freqs [row]
  (frequencies (mapv num-to-row (keys row)))
  )

(defn col-to-freqs [row]
  (frequencies (mapv num-to-col (keys row)))
  )
(defn state-to-rows [{:keys [board zero]}]
  (let [new-board (mapv row-to-freqs board)]
    {:board new-board :zero zero}
    )
  )

(defn state-to-cols [{:keys [board zero]}]
  (let [new-board (mapv col-to-freqs board)]
    {:board new-board :zero zero}
    )
  )


(defn manh-dist [s t]
  (let [c1 (state-coords s)
        c2 (state-coords t)
        ]
    (sum-seq (for [[k v] c1]
               (let [[x1 y1] v
                     [x2 y2] (get c2 k)
                     ]
                 (+ (abs (- x1 x2)) (abs (- y1 y2)))
                 )
               )
             )
    )
  )


(defn next-node [{:keys [state length]} action]
  (let [board (:board state)
        zero (:zero state)
        new-pos (+ zero action)
        ret-val (if (inside? new-pos)
                  ;;
                  (let [
                        new-row (nth board new-pos)
                        uniq-elts (set (keys new-row))
                        cur-row (nth board zero)
                        new-nodes (for [x uniq-elts]
                                    (let [
                                          new-board {:board (assoc (assoc board new-pos (inc-map new-row x)) zero (change-zero-row cur-row x)) :zero new-pos}
                                          ]
                                      {:state new-board :length (+ length 1)}
                                      )
                                    )]
                    (vec new-nodes)) []
                  )]
    ret-val
    ))

(defn expand [node reached q]
  (let [next-nodes (flatten (for [action [UP DOWN]]
                              (next-node node action)))]
    (loop [nn next-nodes p q r reached]
      (if (= (count nn) 0)
        {:queue p :reached r}
        (let [[n & ns] nn
              s (:state n)
              new-vals (if (not (contains? r s))
                         {:reached (assoc r s n) :queue (assoc p n (:length n))}
                         {:reached r :queue p}
                         )
              new-q (:queue new-vals)
              new-r (:reached new-vals)
              ]
          (recur ns new-q new-r)
          )
        ))
    )
  )


(defn get-dists [m]
  (into {} (for [[k v] m] [k ((fn [x] (:length x)) v)])))



(defn node-to-list [node]
  (loop [n node acc []]
    (if (not (some? (:prev n)))
      (conj acc (:state n))
      (recur (:prev n) (conj acc (:state n)))
      )
    )
  )

(defn state-to-string [s]
  (let [
        smaps (map (fn [x] (vals (into (sorted-map) (map-invert x)))) (:board s))
        ]
    (clojure.string/join "\n" smaps)
    )
  )

(defn solution []
  (let [start-state {:board [{1 4}, {2 4}, {3 4}, {4 3, 0 1}] :zero 3}
        start-node {:state start-state :length 0 :prev nil}
        q (pm/priority-map start-node 0)
        reached {start-state start-node}
        ]
    (loop [r reached p q]
      (if (not (empty? p))
        (let [n (nth (peek p) 0)
              next-result (expand n r (pop p))
              new-reached (:reached next-result)
              new-queue (:queue next-result)]
          (recur new-reached new-queue)
          )
        (get-dists r)
        )
      )
    )
  )


(defonce dists 
  (solution)
  )
