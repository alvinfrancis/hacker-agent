(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [hacker-agent.hacker-base :as base :refer [reset-data-sync!]])
  (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {}))

(defn global-state [k & [default]]
  (get @app-state k default))

(defn global-put! [k v]
  (swap! app-state assoc k v))

(defonce state-bound
  (base/bind-top-stories! app-state [:top-stories]))

;; ------------------------
;; Components
(defmulti item :type)

(defmethod item "story" [data]
  (let [local (atom {:collapse? false
                     :collapse-comments? true})]
    (fn [data]
      (let [{:keys [id by title kids type time url score]} data]
        (if (:collapse? @local)
          [:p {:on-click #(swap! local update-in [:collapse?] not)} title]
          [:ul
           [:li "ID: " id]
           [:li {:on-click #(swap! local update-in [:collapse?] not)} title]
           [:li "URL: " url]
           [:li "Score: " score]
           [:li "By: " by]
           [:li "Time: " time]
           (when kids
             [:li [:span {:on-click #(swap! local update-in [:collapse-comments?] not)}
                   "Comments: "]
              (when-not (:collapse-comments? @local)
                (for [[id sub-data] (vec kids)]
                  ^{:key id} [item sub-data]))])])))))

(defmethod item "comment" [data]
  (let [local (atom {:collapse-comments? true})]
    (fn [data]
      (let [{:keys [id by kids parent text type time score deleted]} data]
        (when-not deleted
          [:ul
           [:li "ID: " [:a {:href (str "/#/items/" id)} id]]
           [:li "Parent: " [:a {:href (str "/#/items/" parent)} parent]]
           [:li "By: " by]
           [:li {:dangerouslySetInnerHTML {:__html (str "Text: </br>" text)}}]
           [:li "Time: " time]
           (when kids
             [:li [:span {:on-click #(swap! local update-in [:collapse-comments?] not)}
                   "Comments: "]
              (when-not (:collapse-comments? @local)
                (for [[id sub-data] (vec kids)]
                  ^{:key id} [item sub-data]))])])))))

(defmethod item "job" [data]
  [:p "This is a job"])

(defmethod item nil [_]
  [:p "Cannot render item"])

(defn top-stories [state]
  (let [{top-stories :top-stories} state]
    [:ul
     (for [id top-stories]
       ^{:key id} [:li [:a {:href (str "/#/items/" id)} id]])]))

;; -------------------------
;; Views
(defmulti page :render-view)

(defmethod page nil [_]
  [:div "Invalid/Unknown route"])

(defmethod page :main [state]
  [:div
   [:h3 "Top Stories"]
   [top-stories state]])

(defmethod page :item [state]
  (when-let [entry (get-in state [:current-item :item])]
    [item entry]))

(defn main-page [state]
  [:div
   [:h2 "Hacker News"]
   (page @state)])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (swap! app-state assoc
         :render-view :main))

(secretary/defroute "/items/:id" [id]
  (swap! app-state assoc
         :render-view :item)
  (reset-data-sync! id app-state [:current-item]))

;; -------------------------
;; Initialize app
(defn init! []
  (reagent/render-component [main-page app-state] (.getElementById js/document "app")))

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
(defonce hooked
  (hook-browser-navigation!))
