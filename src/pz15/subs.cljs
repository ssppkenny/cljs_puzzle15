(ns pz15.subs
  (:require
   [re-frame.core :as re-frame]
   [re-pressed.core :as rp]
   ))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::keydown-keys
 (fn [db _]
   (get-in db [:re-pressed.core/keydown :keys])))

(re-frame/reg-sub
 ::solving
 (fn [db]
   (:solving db)))

(re-frame/reg-sub
 ::need-to-solve
 (fn [db]
   (:need-to-solve db)))

(re-frame/reg-sub
 ::solution
 (fn [db]
   (:solution db)))

(re-frame/reg-sub
 ::state
 (fn [db]
   (:state db)))

(re-frame/reg-sub
 ::re-pressed-example
 (fn [db _]
   (:re-pressed-example db)))
