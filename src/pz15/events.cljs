(ns pz15.events
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [pz15.db :as db]
   [pz15.bfs :as bfs]
   [clojure.core.async :as ca]))

(re-frame/reg-event-db
  ::initialize-db
  (fn [_ _]
    db/default-db))

;; -- [Helper functions for solution animation] --

(defn states-move [[prev_state state]]
  (let [prev_board (:board prev_state)
        board (:board state)
        prev_zero (:zero prev_state)
        zero (:zero state)]
    (if (not= zero prev_zero)
      (- zero prev_zero)
      (let [row (nth board zero)
            prev_row (nth prev_board zero)
            zero_prev (get prev_row 0)
            zero_n (get row 0)]
        (* (- zero_n zero_prev) 10)))))

(defn action-to-event [a]
  (cond
    (= a db/UP) ::timeout-up
    (= a db/DOWN) ::timeout-down
    (= a db/LEFT) ::timeout-left
    (= a db/RIGHT) ::timeout-right))

(defn states-to-events [states]
  (let [pairs (map vector (rest states) states)
        events (reverse (mapv states-move pairs))
        key-events (mapv action-to-event events)]
    (concat (map-indexed (fn [i e]
                           {:dispatch [e 1]
                            :ms (* 500 i)})
                         key-events)
            [{:dispatch [::timeout-solved]
              :ms (* 500 (count key-events))}])))

(re-frame.core/reg-event-fx
  ::play-solution
  (fn [{:keys [db]} [_ sol]]
    (if (and (vector? sol) (> (count sol) 1))
      {:db (assoc db :solving true)
       :fx (conj (mapv (fn [{:keys [dispatch ms]}]
                         [:dispatch-later {:ms ms :dispatch dispatch}])
                       (states-to-events sol))
                  [:dispatch-later {:ms (* 128 (dec (count sol))) :dispatch [::stop-solve]}])}
      {:db (assoc db :solving false)})))

;; Solution animation timeout-move handlers
(re-frame/reg-event-fx
  ::timeout-left
  (fn [{:keys [db]} [_]]
    {:db (update db :state db/move-left)}))

(re-frame/reg-event-fx
  ::timeout-right
  (fn [{:keys [db]} [_]]
    {:db (update db :state db/move-right)}))

(re-frame/reg-event-fx
  ::timeout-up
  (fn [{:keys [db]} [_]]
    {:db (update db :state db/move-up)}))

(re-frame/reg-event-fx
  ::timeout-down
  (fn [{:keys [db]} [_]]
    {:db (update db :state db/move-down)}))

;; [Keyboard, shuffle, and solver event handlers - must be present for input/animation to work]

(re-frame/reg-event-db
  ::key-left
  (fn [db [_ & args]]
    (if (or (not (:solving db)) (seq args))
      (update db :state db/move-left)
      db)))

(re-frame/reg-event-db
  ::key-right
  (fn [db [_ & args]]
    (if (or (not (:solving db)) (seq args))
      (update db :state db/move-right)
      db)))

(re-frame/reg-event-db
  ::key-up
  (fn [db [_ & args]]
    (if (or (not (:solving db)) (seq args))
      (update db :state db/move-up)
      db)))

(re-frame/reg-event-db
  ::key-down
  (fn [db [_ & args]]
    (if (or (not (:solving db)) (seq args))
      (update db :state db/move-down)
      db)))

(re-frame/reg-event-db
  ::shuffle
  (fn [db _]
    (assoc db :state (db/random-state)
              :solution []
              :solving false
              :need-to-solve false
              :bfs-state nil)))

(re-frame/reg-event-fx
  ::new-solve
  (fn [{:keys [db]} _]
    (if (not (:solving db))
      {:db (assoc db :solving true
                  :need-to-solve true
                  :solution []
                  :bfs-state nil)
       :dispatch [::do-new-solve]}
      {:db db})))

(re-frame/reg-event-fx
  ::do-new-solve
  (fn [{:keys [db]} _]
    (let [bfs-state (bfs/bfs-init (:state db))]
      {:db (assoc db :bfs-state bfs-state)
       :dispatch [::solver-step]})))

(re-frame/reg-event-fx
  ::solver-step
  (fn [{:keys [db]} _]
    (let [bfs-state (bfs/bfs-step (:bfs-state db) 10)]
      (if (:done? bfs-state)
        ;; Solution (or unsolvable) found
        (let [sol (:solution bfs-state)]
          {:db (assoc db
                      :solution sol
                      :solving false
                      :bfs-state nil
                      :need-to-solve false)
           :dispatch (when (seq sol) [::play-solution sol])})
        ;; Continue chunking
        {:db (assoc db :bfs-state bfs-state)
         :dispatch-later [{:ms 0 :dispatch [::solver-step]}]}))))

(re-frame/reg-event-db
  ::move-tile
  (fn [db [_ tile-number]]
    (let [state    (:state db)
          board    (:board state)
          zero     (:zero state)
          zero-col (get (nth board zero) 0)
          tile-row (first (keep-indexed (fn [i row] (when (contains? row tile-number) i)) board))
          tile-col (get (nth board tile-row) tile-number)
          direction (cond
                      (and (= tile-col zero-col) (= tile-row (+ zero 1))) db/UP
                      (and (= tile-col zero-col) (= tile-row (- zero 1))) db/DOWN
                      (and (= tile-row zero) (= tile-col (+ zero-col 1))) db/LEFT
                      (and (= tile-row zero) (= tile-col (- zero-col 1))) db/RIGHT)]
      (if (and direction (not (:solving db)))
        (assoc db :state (db/next-state-for-action state direction))
        db))))

(re-frame/reg-event-db
  ::stop-solve
  (fn [db _]
    (assoc db :solving false :need-to-solve false)))

(re-frame/reg-event-db
  ::timeout-solved
  (fn [db _]
    (assoc db :solving false :need-to-solve false)))
