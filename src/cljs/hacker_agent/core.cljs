(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close!]]
            [hacker-agent.session :as session :refer [global-state global-put!]])
  (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {:text "Hello, this is: "}))

(defonce firebase
  (let [fb (js/Firebase. "https://hacker-news.firebaseio.com/v0")]
    (.on (.child fb "topstories") "value"
         (fn [snapshot]
           (let [db (js->clj (.val snapshot))]
             (global-put! :stories db))))
    (.on (.child fb "maxitem") "value"
         (fn [snapshot]
           (let [db (js->clj (.val snapshot))]
             (global-put! :max-item db))))
    fb))

;; -------------------------
;; Components

(defn hacker []
  (let [fb (js/Firebase. "https://hacker-news.firebaseio.com/v0/topstories")]
    [:div
     [:h2 "Home Page"]
     [:div
      [:h3 "Latest Item"]
      [:div
       (global-state :max-item)]]
     [:div
      [:h3 "Top Stories"]
      [:ul
       (for [item (global-state :stories)]
         ^{:key item} [:li item])]]]))

;; -------------------------
;; Views

(defmulti page identity)

(defmethod page :default [_]
  [:div "Invalid/Unknown route"])

(defmethod page :hacker [_]
  [hacker])

(defn main-page []
  [:div [page :hacker]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (swap! app-state assoc :current-page :hacker))

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [main-page] (.getElementById js/document "app")))

;; -------------------------
;; History
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))
;; need to run this after routes have been defined
(hook-browser-navigation!)
