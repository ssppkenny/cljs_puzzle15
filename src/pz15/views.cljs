(ns pz15.views
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   [reagent.core :as reagent]
   [clojure.core.async :as ca]
   [pz15.events :as events]
   [pz15.subs :as subs]
   [pz15.db :as db]
   [pz15.bfs :as bfs]
   ))

(defn dispatch-keypress-rules []
  (re-frame/dispatch
   [::rp/set-keypress-rules
    {:event-keys [
                  [[::events/key-left 1]
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
                    ]]
                  ]

     :clear-keys
     [[{:keyCode 27} ;; escape
       ]]}]))
(defn dispatch-keydown-rules []
  (re-frame/dispatch
   [::rp/set-keydown-rules
    {:event-keys [
                  [[::events/shuffle 1]
                   [{:keyCode 32} ;;
                    ]
                   ]
                  [[::events/key-left 1]
                   [{:keyCode 72} ;;
                    ]
                   [{:keyCode 37} ;;
                    ]
                   ]
                  [[::events/key-right 1]
                   [{:keyCode 76} ;; 
                    ]
                   [{:keyCode 39} ;; 
                    ]
                   ]
                  [[::events/key-up 1]
                   [{:keyCode 75} ;; 
                    ]
                   [{:keyCode 38} ;; 
                    ]
                   ]
                  [[::events/new-solve 1]
                   [{:keyCode 83} ;; 
                    ]
                   ]
                  [[::events/key-down 1]
                   [{:keyCode 74} ;; 
                    ]
                   [{:keyCode 40} ;; 
                    ]
                   ]
                  ]

     :clear-keys
     [[{:keyCode 27} ;; escape
       ]]}]))



(defn main-panel []
  (let [state (re-frame/subscribe [::subs/state])
         need-to-solve (re-frame/subscribe [::subs/need-to-solve])
         solving (re-frame/subscribe [::subs/solving])
        ]
    (let [field state]
      (reagent/create-class
       {:component-did-mount
         (fn [] (dispatch-keydown-rules)
           )
        :reagent-render
         (fn []
         (let [
                  show-progress @solving  
               ]
        [:div {:style {:display "flex" :justify-content "center"}}
        [:div {:style {:width "205px" :border-style "solid" :border-width "1px" }} [:div {:style {:text-align "center"}} [:span {:style {:font-size "25px"}} "15 Puzzle" ]  ]
        [:div {:style {:border-style "solid" :width "fit-content" :border-width "2px" }} 
        (for [l (:board @field)]
          ^{:key (random-uuid)} [:div {:style {:display "flex" :width "100%" :height "50px" :text-align "center"}} (for [[n i] l]
            ^{:key (random-uuid)} [:div {:display "inline-block" :style {:width "50px" :border-style "none" :border-width "1px" :color "black" :margin "0px"} } 
(if (not (= n 0))
    [:img {:src (str "images/t" n ".png" ) :width "50px" :height "50px" }]
    [:img {:src "images/empty.png" :width "50px" :height "50px" }]
        )
             ]
            )]
          )

       ]
         [:div {:style {:display "inline-block"}} 
             (if show-progress [:img {:src "images/wait.gif" :width "30px"}] [:div])
          ]
         [:div (if (= @field bfs/target-state) "Solved!!!" "")] 
         [:div {:style {:display "block"}} 
            [:img {:src "images/up.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-up 1] ) )}] 
            [:img {:src "images/down.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-down 1]) )}] 
            [:img {:src "images/left.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-left 1]) )}] 
            [:img {:src "images/right.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/key-right 1]) )}] 
          ]
         [:div {:style {:text-align "center"}} 
            [:img {:src "images/solve.jpg" :width "50px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/new-solve 1]) )}] 
          [:span "Press s to solve"]
          [:p]
            [:img {:src "images/space.png" :width "150px" :on-click (fn [e] (.preventDefault e) (re-frame/dispatch [::events/shuffle 1]) )}] 
          [:p]
          [:span "Press space to shuffle"]
          ]
        ]]
  )
  )
         })
      )
    )
  )
