(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [hacker-agent.hacker-base :as base]
            [hacker-agent.time :as t])
  (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {}))

(defonce stories-synced
  (base/bind! app-state [:top-stories] base/top-stories base/stories-binder))

;; ------------------------
;; Components
(defn nav []
  [:div.nav
   [:h2 "Hacker News"]
   [:ul
    [:li [:a {:href "#/"} "Top"]]
    [:li [:a {:href "#"} "New"]]
    [:li [:a {:href "#"} "Threads"]]
    [:li [:a {:href "#"} "Comments"]]
    [:li [:a {:href "#"} "Show"]]]])

(defn comment [data]
  (let [local (atom {:collapse-comments? false})]
    (fn [data]
      [:div.item
       (let [{:keys [id by kids parent text type time score deleted]} data]
         (when-not deleted
           (if (and data (not id))
             [:p "Loading..."]
             [:ul
              {:on-click #(do
                            (swap! local update-in [:collapse-comments?] not)
                            (.stopPropagation %))
               :style {:cursor "pointer"}}
              [:li
               [:p
                (str by " | " (t/time-ago time) " | ")
                [:a {:href (str "/#/items/" id)} "link"]
                [:p {:dangerouslySetInnerHTML {:__html text}}]]]
              (when kids
                (if (:collapse-comments? @local)
                  [:p [:b (count kids) " comments"]]
                  (for [[id sub-data] (vec kids)]
                    ^{:key id} [comment sub-data])))])))])))

(defn comment-list [comments]
  [:ul
   (for [[id sub-data] comments]
     ^{:key id} [comment sub-data])])

(defn story [data]
  (let [{:keys [id by title kids type time url score]} data]
    (when id
      [:div
       [:p [:a {:href url} title]]
       [:p
        (str score " points by " by)
        " | "
        [:a {:href (str "/#/items/" id)}
         (str (count kids) " comments")]]
       (when kids
         [comment-list (vec kids)])])))

(defn story-list-item [story]
  (let [{:keys [by id title score url kids]} story]
    (when (every? identity [by title score])
      [:div
       [:p [:a {:href url} title]]
       [:p
        (str score " points by " by)
        " | "
        [:a {:href (str "/#/items/" id)}
         (str (count kids) " comments")]
        " | "
        [:span {:on-click #(base/bind! app-state [:current-item]
                                       (base/id->fbref id)
                                       base/item-binder)
                :style {:cursor :pointer}}
         [:i "Preview"]]]])))

(defn top-stories [state]
  (let [{top-stories :top-stories} state
        selected-story (get-in state [:current-item])]
    [:ol
     (for [[index entry] (into (sorted-map) top-stories)]
       (let [id (entry :id)]
         (if (= id (:id selected-story))
           ^{:key index} [:li
                          [story-list-item entry]
                          [comment-list (vec (:kids selected-story))]]
           ^{:key index} [:li [story-list-item entry]])))]))

;; -------------------------
;; Views
(defmulti page :render-view)

(defmethod page nil [_]
  [:div "Invalid/Unknown route"])

(defmethod page :main [state]
  [top-stories state])

(defmethod page :item [state]
  (when-let [entry (get-in state [:current-item])]
    (case (:type entry)
      "story" [story entry]
      "comment" [comment entry]
      [:p "Cannot render item"])))

(defn main-page [state]
  [:div
   [nav]
   (page @state)])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (swap! app-state assoc
         :render-view :main)
  (base/unbind! app-state [:current-item]))

(secretary/defroute "/items/:id" [id]
  (swap! app-state assoc :render-view :item)
  (base/bind! app-state [:current-item]
              (base/id->fbref id)
              base/item-binder))

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
