(ns hacker-agent.debug
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.set :as set]
            [clojure.data :as data]
            [secretary.core :as secretary :include-macros true]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close!]]
            [hacker-agent.utils :as utils]))

(defonce show? (atom false))

(defonce debug-chan (chan))

(declare field field-list)

(defn console [state]
  (when @show?
    [:div.console
     [:h4 "Debug Console"]
     [field-list @state]]))

;; Proxy into console for live reload
(defn view [state]
  [console state])

(defn field [field]
  (let [collapse? (atom true)]
    (fn [field]
      (let [[k v] field]
        [:div
         [:p [:b (when-not @collapse?
                   {:on-click #(reset! collapse? true)
                    :style {:cursor :pointer
                            :text-decoration :underline}})
              (clj->js k)]
          ": "
          (if @collapse?
            (if (map? v)
              [:span {:on-click #(reset! collapse? false)
                      :style {:cursor :pointer}}
               "Object"]
              (clj->js v))
            (if (map? v)
              [field-list v]
              (clj->js v)))]]))))

(defn field-list [fields]
  [:ul
   (for [f (into (sorted-map) fields)]
     ^{:key (key f)} [:li [field f]])])

(defn init! [state]
  (r/render-component [view state] (.getElementById js/document "debug")))

(defonce key-chan (utils/listen (dom/getDocument) (.-KEYPRESS events/EventType)))

(defonce key-loop
  (go-loop []
    (when-let [event (<! key-chan)]
      (when (= (.. event -keyCode) 4)   ; CTRL-D
        (swap! show? not))
      (recur))))
