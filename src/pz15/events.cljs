(ns pz15.events
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [pz15.db :as db]
   [pz15.bfs :as bfs]
   [clojure.core.async :as ca]
   ))


(defonce ^:const UP 1)

(defonce ^:const DOWN -1)

(defonce ^:const LEFT 10)

(defonce ^:const RIGHT -10)

(defn states-move [pair]
  (let [[prev_state state] pair
        prev_board (:board prev_state)
        board (:board state)
        prev_zero (:zero prev_state)
        zero (:zero state)]
    (if (not (= zero prev_zero))
      (- zero prev_zero)
      (let [row (nth board zero)
            prev_row (nth prev_board zero)
            zero_prev (get prev_row 0)
            zero_n (get row 0)]
        (* (- zero_n zero_prev) 10)))))

(defn action-to-event [a]
  (cond
    (= a UP) ::timeout-up
    (= a DOWN) ::timeout-down
    (= a LEFT) ::timeout-left
    (= a RIGHT) ::timeout-right))

(defn states-to-events [states]
  (let [pairs (map vector (rest states) states)
        events (reverse (mapv action-to-event (mapv states-move pairs)))]
    (conj (map-indexed (fn [a b] (vector b (* 500 a)))  events) [::timeout-solved (* (count states) 500)] )
    )
)

(re-frame/reg-event-fx
  ::timeout-left
  (fn [{:keys [event db]} arg]
    (.log js/console event)
    {:db (assoc db :need-to-solve false :solving true)
     :fx [ [:dispatch-later {:ms (nth event 1) :dispatch [::key-left 0]}]]
     }
    )
  )

(re-frame/reg-event-fx
  ::timeout-right
  (fn [{:keys [event db]} arg]
    (.log js/console event)
    {:db (assoc db :need-to-solve false :solving true)
     :fx [ [:dispatch-later {:ms (nth event 1) :dispatch [::key-right 0]}]]
     }
    )
  )

(re-frame/reg-event-fx
  ::timeout-up
  (fn [{:keys [event db]} arg]
    (.log js/console event)
    {:db (assoc db :need-to-solve false :solving true)
     :fx [ [:dispatch-later {:ms (nth event 1) :dispatch [::key-up 0]}]]
     }
    )
  )

(re-frame/reg-event-fx
  ::timeout-solved
  (fn [{:keys [db event]} arg]
    (.log js/console event)
    {:db (assoc db :need-to-solve false :solving true)
     :fx [ [:dispatch-later {:ms (nth event 1) :dispatch [::stop-solve 0]}]]
     }
    )
  )

(re-frame/reg-event-fx
  ::timeout-down
  (fn [{:keys [db event]} arg]
    (.log js/console event)
    {:db (assoc db :need-to-solve false)
     :fx [ [:dispatch-later {:ms (nth event 1) :dispatch [::key-down 0]}]]
     }
    )
  )

(re-frame/reg-event-db
  ::stop-solve
  (fn [db]
      (assoc db :need-to-solve false :solving false)
    )
  )

(re-frame/reg-event-fx
  ::do-solve
  (fn [{:keys [db event]} arg]
      (let [solution (bfs/bfs (:state db))
            events (states-to-events solution)
            fx (mapv (fn [[e d] arg] [:dispatch-later {:ms d :dispatch [e 0]} ]) events ) 
            ]
        {:db (assoc db :solving true)
         :fx fx
         }
      )
    )
  )

(re-frame/reg-event-fx
  ::play-solution
  (fn [{:keys [db event]} arg]
    (let [[ev sol] event
           events (states-to-events sol)
           fx (mapv (fn [[e d] arg] [:dispatch-later {:ms d :dispatch [e 0]} ]) events ) 
          ]
        {:db db 
         :fx fx 
         }
      )
    )
  )


(re-frame/reg-event-fx
  ::do-new-solve
  (fn [{:keys [db event]} arg]
        (let [ c (ca/chan 10) ]
         (ca/go (ca/put! c (:state db) (fn [s] (re-frame/dispatch [::play-solution (bfs/bfs (:state db))]
                                   ))))
           
        {:db (assoc db :solving true)
         }
    )
))

(re-frame/reg-event-fx
  ::new-solve
  (fn [{:keys [db event]} arg]
        {:db (assoc db :solving true)
         :fx [ [:dispatch-later {:ms 100 :dispatch [::do-new-solve 0]}] ]
         }
    )
)


(re-frame/reg-event-fx
  ::solve
  (fn [{:keys [db event]} arg]
        {:db (assoc db :solving true)
         :fx [[:dispatch-later {:ms 100 :dispatch [::do-solve 0]}]]
         }
    )
  )

(defn move [db event f]
    (let [
          flag (or (= (nth event 1) 0) (not (:solving db)))
          new-state (f (:state db))
          ]
          (if flag 
             (assoc db :state new-state)
              db  
          )
      ) 
  )

(re-frame/reg-event-db
  ::key-down
  (fn [db event]
    (move db event db/move-down)
    )
  )
(re-frame/reg-event-db
  ::key-up
  (fn [db event]
    (move db event db/move-up)
    )
  )

(re-frame/reg-event-db
  ::key-right
  (fn [db event]
    (move db event db/move-right)
    )
  )

(re-frame/reg-event-db
  ::key-left
  (fn [db event]
    (move db event db/move-left)
    )
  )

(re-frame/reg-event-db
  ::shuffle
  (fn [db]
    (let [
          new-state (db/random-state)
          ]
          (if (not (:solving db))
            (assoc db :state new-state :need-to-solve false)
            db
          )
      ) 
    )
  )

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

