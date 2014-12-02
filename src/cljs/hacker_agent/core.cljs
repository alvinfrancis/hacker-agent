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

(defonce hacker-root "https://hacker-news.firebaseio.com/v0")
;; -------------------------
;; Utils
(defn snapshot->clj [snapshot]
  (js->clj (.val snapshot) :keywordize-keys true))

(defn id->fbref [id]
  (.. (js/Firebase. hacker-root)
      (child "item")
      (child id)))

(defn- fb->chan
  ([fbref]
   (let [events [:child_added :child_changed :child_removed]
         chans (map (fn [event]
                      (fb->chan fbref event))
                    events)]
     (merge chans)))
  ([fbref event]
   (let [c (chan)]
     (.on fbref (clojure.core/name event)
          (fn [snapshot]
            (put! c [event
                     (keyword (.key snapshot))
                     (js->clj (.val snapshot))])))
     c)))

(defn- fb->atom [fbref]
  (let [c (fb->chan fbref)
        data (atom {})]
    (go-loop [msg (<! c)]
      (let [[event key val] msg]
        (case event
          :child_added (swap! data assoc key val)
          :child_changed (swap! data assoc key val)
          :child_removed (swap! data dissoc key)))
      (recur (<! c)))
    data))

(defn listen
  "Listens for events on the given firebase ref"
  [root korks]
  (let [root (p/walk-root root korks)
        events [:child_added :child_removed :child_changed]
        td (map (fn [[evt snap]]
                  [evt (.name snap) (.val snap)]))
        chans (map (fn [event]
                     (fb->chan root event)) events)]
    (merge chans)))

;; -------------------------
;; State

(defonce app-state (atom {}))

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
                       (let [curr (snapshot->clj snapshot)]
                         (global-put! :current-item curr)))))
             (global-put! :max-item db))))
    fb))

;; -------------------------
;; Components

(defn hacker []
  [:div
   [:h2 "Home Page"]
   [:div
    [:h3 "Test Story"]
    [item 8678847]
    ]
   [:div
    [:h3 "Top Stories"]
    [:ul
     (for [story (global-state :stories)]
       ^{:key story} [:li [item story]])]
    ]])

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
