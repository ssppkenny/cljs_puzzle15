(ns pz15.db
  (:require
   [clojure.set :refer [map-invert]]
   [shadow.resource :as rc]
   [cljs.reader :as rdr]))

(defn read-dists []
  (.log js/console "log")
  (let [f (rc/inline "pz15/data.edn")]
    (rdr/read-string f)))

(defonce dists
  (read-dists))

(defonce ^:const UP 1)

(defonce ^:const DOWN -1)

(defonce ^:const LEFT 10)

(defonce ^:const RIGHT -10)

(defn sum-seq [s]
  (reduce + s))

(defn count-greater [n v]
  (sum-seq (for [[i x] v]
             (if (> n x) 1 0))))

(defn even-perm? [s]
  (let [v (vec (map-indexed (fn [a b] [(+ a 1) b]) s))
        s (sum-seq (for [[i n] v]
                     (count-greater n (subvec v i))))]

    (= (rem s 2) 0)))

(defn random-state []
  (loop [v (shuffle (range 16))]
    (let [zero (quot (.indexOf v 0) 4)
          f (filter (fn [x] (> x 0)) v)]
      (if (or (and (even-perm? f) (= (rem (+ zero 1) 2) 0))
              (and (not (even-perm? f)) (> (rem (+ zero 1) 2) 0)))
        {:board
         (mapv (fn [x] (zipmap x (range 4)))
               (mapv vec (partition 4 v))) :zero zero}
        (recur (shuffle (range 16)))))))

(defonce target-state
  {:board
   (mapv (fn [x] (zipmap x (range 4)))
         (mapv vec (partition 4 (map (fn [x] (rem (+ x 1) 16)) (range 16))))) :zero 3})

(defn swap-elts [v i1 i2] (assoc v i2 (v i1) i1 (v i2)))

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

(defn next-state-for-action [{:keys [board zero]} action]
  (let [zero-row (nth board zero)
        zero-pos (get zero-row 0)
        next-s (cond
                 (= action UP) (if (inside? (+ zero 1)) {:board (switch-vert board zero (+ zero 1)) :zero (+ zero 1)} nil)
                 (= action DOWN) (if (inside? (- zero 1)) {:board (switch-vert board zero (- zero 1)) :zero (- zero 1)} nil)
                 (= action LEFT) (if (inside? (+ zero-pos 1)) {:board (switch-hor board zero (+ zero-pos 1)) :zero zero} nil)
                 :else (if (inside? (- zero-pos 1)) {:board (switch-hor board zero (- zero-pos 1)) :zero zero} nil))]

    (if (some? next-s)
      {:board (mapv sort-row-by-value (:board next-s)) :zero (:zero next-s)}
      nil)))

(defn move-right [s]
  (let [new-state (next-state-for-action s RIGHT)]
  ;;(let [new-state s]
    (if (some? new-state)
      new-state s)))

(defn move-left [s]
  (let [new-state (next-state-for-action s LEFT)]
  ;;(let [new-state s]
    (if (some? new-state)
      new-state s)))

(defn move-down [s]
  (let [new-state (next-state-for-action s DOWN)]
  ;;(let [new-state s]
    (if (some? new-state)
      new-state s)))

(defn move-up [s]
  (let [new-state (next-state-for-action s UP)]
  ;;(let [new-state s]
    (if (some? new-state)
      new-state s)))

(def default-db
  {:name "re-frame"
   :state (random-state)
   :solution []
   :solving false
   :need-to-solve false})
