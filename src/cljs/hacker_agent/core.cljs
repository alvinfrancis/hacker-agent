(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [pani.cljs.core :as p]
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
             (.. fb
                 (child "item")
                 (child db)
                 (on "value"
                     (fn [snapshot]
                       (let [curr (js->clj (.val snapshot) :keywordize-keys true)]
                         (global-put! :current-item curr)))))
             (global-put! :max-item db))))
    fb))

;; -------------------------
;; Components
(defmulti hacker-item (fn [item]
                        (if-let [type (#{"comment" "story"} (:type item))]
                          type
                          :default)))

(defmethod hacker-item "comment"
  [item]
  (let [{:keys [id by text type time]} item]
    [:ul
     [:li "ID: " id]
     [:li "By: " by]
     [:li {:dangerouslySetInnerHTML {:__html (str "Text: " text)}}]
     [:li "Time: " time]]))

(defmethod hacker-item "story"
  [item]
  [:p "TODO: render story item"])

(defmethod hacker-item :default
  [item]
  [:p "Item cannot be rendered"])

(defn hacker []
  (let [fb (js/Firebase. "https://hacker-news.firebaseio.com/v0/topstories")]
    [:div
     [:h2 "Home Page"]
     [:div
      [:h3 "Latest Item"]
      [hacker-item (global-state :current-item)]]
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
