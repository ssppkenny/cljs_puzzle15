(ns pz15.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [reagent.core :as reagent]
   [clojure.core.async :as ca]
   [pz15.events :as events]
   [pz15.subs :as subs]
   [pz15.db :as db]
   [pz15.bfs :as bfs]))

(defn dispatch-keypress-rules []
  (re-frame/dispatch
   [::rp/set-keypress-rules
    {:event-keys [[[::events/key-left 1]
                   [{:keyCode 72} ;;
                    ]]
                  [[::events/key-right 1]
                   [{:keyCode 76} ;; 
                    ]]
                  [[::events/key-up 1]
                   [{:keyCode 75} ;; 
                    ]]
                  [[::events/key-down 1]
                   [{:keyCode 74} ;; 
                    ]]]
     :clear-keys
     [[{:keyCode 27} ;; escape
       ]]}]))
(defn dispatch-keydown-rules []
  (re-frame/dispatch
   [::rp/set-keydown-rules
    {:event-keys [[[::events/shuffle 1]
                   [{:keyCode 32} ;;
                    ]]
                  [[::events/key-left 1]
                   [{:keyCode 72} ;;
                    ]
                   [{:keyCode 37} ;;
                    ]]
                  [[::events/key-right 1]
                   [{:keyCode 76} ;; 
                    ]
                   [{:keyCode 39} ;; 
                    ]]
                  [[::events/key-up 1]
                   [{:keyCode 75} ;; 
                    ]
                   [{:keyCode 38} ;; 
                    ]]
                  [[::events/new-solve 1]
                   [{:keyCode 83} ;; 
                    ]]
                  [[::events/key-down 1]
                   [{:keyCode 74} ;; 
                    ]
                   [{:keyCode 40} ;; 
                    ]]]
     :clear-keys
     [[{:keyCode 27} ;; escape
       ]]}]))
(defn adjacent-to-zero? [board zero n]
  (let [zero-col (get (nth board zero) 0)
        tile-row (first (keep-indexed (fn [i row] (when (contains? row n) i)) board))
        tile-col (get (nth board tile-row) n)]
    (or (and (= tile-col zero-col) (= tile-row (+ zero 1)))
        (and (= tile-col zero-col) (= tile-row (- zero 1)))
        (and (= tile-row zero) (= tile-col (+ zero-col 1)))
        (and (= tile-row zero) (= tile-col (- zero-col 1))))))

(defn render-cell [board zero dragged-tile n]
  (let [is-empty    (= n 0)
        is-adjacent (and (not is-empty) (adjacent-to-zero? board zero n))]
    [:div.cell
     (merge
      {:class (str (when is-adjacent "draggable") (when is-empty "drop-target"))}
      (when is-adjacent
        {:draggable     true
         :on-drag-start (fn [e]
                          (reset! dragged-tile n)
                          (.setData (.-dataTransfer e) "text/plain" (str n)))})
      (when is-empty
        {:on-drag-over (fn [e] (.preventDefault e))
         :on-drop      (fn [e]
                         (.preventDefault e)
                         (when @dragged-tile
                           (re-frame/dispatch [::events/move-tile @dragged-tile])
                           (reset! dragged-tile nil)))}))
     (if (not is-empty)
       [:img {:src (str "images/t" n ".png") :width "50px" :height "50px"}]
       [:img {:src "images/empty.png" :width "50px" :height "50px"}])]))

(defn render-row [board zero dragged-tile l]
  (into [:div.row]
        (map (fn [[n _]] ^{:key n} [render-cell board zero dragged-tile n]) l)))

(defn main-panel []
  (let [state        (re-frame/subscribe [::subs/state])
        solving      (re-frame/subscribe [::subs/solving])
        dragged-tile (reagent/atom nil)]
    (reagent/create-class
     {:component-did-mount
      (fn [] (dispatch-keydown-rules))
      :reagent-render
      (fn []
         (let [show-progress @solving
               board         (:board @state)
               zero          (:zero @state)]
          [:div.main-container
           [:div.title-box
            [:div.title [:span "15 Puzzle"]]]
           (into [:div.game-box]
                 (map-indexed (fn [i l] ^{:key i} [render-row board zero dragged-tile l]) board))
           [:div.status
            (if show-progress [:img {:src "images/wait.gif" :width "30px"}] [:div])
             (let [start-time (re-frame/subscribe [::subs/solve-start-time])
                   end-time   (re-frame/subscribe [::subs/solve-end-time])]
               (cond
                 (and (= @state bfs/target-state) @end-time @start-time)
                 (let [dt (js/Math.round (* 10 (/ (- @end-time @start-time) 1000))) ; deci-seconds
                       sec (/ dt 10)]
                   [:span (str "Solved in " sec " seconds")])
                 (= @state bfs/target-state)
                 [:span "Solved!!!"]
                 :else
                 [:span ""]))]
           [:div.controls
            [:img {:src "images/up.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-up 1]))}]
            [:img {:src "images/down.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-down 1]))}]
            [:img {:src "images/left.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-left 1]))}]
            [:img {:src "images/right.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-right 1]))}]]
           [:div.actions
            [:img {:src "images/solve.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/new-solve 1]))}]
            [:span "Press s to solve"]
            [:br]
            [:img {:src "images/space.png" :width "150px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/shuffle 1]))}]
            [:br]
            [:span "Press space to shuffle"]]]))})))
