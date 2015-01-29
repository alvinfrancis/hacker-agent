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
            [hacker-agent.utils :as utils]
            [hacker-agent.time :as t]))

(defonce show? (atom false))

(defonce debug-chan (chan))

(declare field field-list)

(defn slider [value min max]
  [:input {:type "range" :value @value :min min :max max
           :on-change #(let [new-val (-> % .-target .-value)]
                         (reset! value (int new-val)))}])

(defn edit-value [value]
  (if (< 50 (count (str @value)))
    [:textarea {:rows 3
                :value @value
                :on-change #(reset! value (-> % .-target .-value))}]
    [:input {:type "text"
             :value @value
             :on-change #(reset! value (-> % .-target .-value))}]))

(defn field [k v]
  (let [collapse? (atom true)]
    (fn [k v]
      [:div
       [:p [:b (when-not @collapse?
                 {:on-click #(reset! collapse? true)
                  :style {:cursor :pointer
                          :text-decoration :underline}})
            (clj->js k)]
        ": "
        (if @collapse?
          (if (map? @v)
            [:span {:on-click #(reset! collapse? false)
                    :style {:cursor :pointer}}
             "Object"]
            (if (= k :score)
              [slider v 0 1000]
              [edit-value v]))
          (if (map? @v)
            [field-list v]
            (clj->js v)))]])))

(defn field-list [fields]
  [:ul
   (for [f (into (sorted-map) @fields)]
     (let [[k v] f]
       ^{:key k} [:li
                  [field k (r/wrap v swap! fields assoc k)]]))])

(defonce state-history (atom []))

(defonce track-history? (atom true))

(defn init-state-tracker! [state]
  (reset! state-history [[(t/now) @state]])
  (go-loop []
    (<! (async/timeout 100))
    (when (and @track-history?
               (not (= (second (last @state-history))
                       @state)))
      (swap! state-history conj [(t/now) @state]))
    (recur)))

(defn slider-atom [state history]
  (let [pos (first (keep-indexed #(when (= %2 @state) %1)
                                 (map second @history)))
        [current-time current-state] (nth @history (int pos))]
    [:div
     [:p (t/time-ago current-time)]
     [:input {:type "range" :value pos :max (dec (count @history))
              :on-change #(let [new-val (-> % .-target .-value)
                                [_ target-state] (nth @history (int new-val)
                                                      [nil @state])]
                            (reset! state target-state))}]
     [:button {:type :button
               :on-click #(let [prev-pos (dec pos)
                                [_ target-state] (nth @history prev-pos
                                                      [nil @state])]
                            (when (>= prev-pos 0)
                              (reset! state target-state)))}
      "<"]
     [:button {:type :button
               :on-click #(let [next-pos (inc pos)
                                [_ target-state] (nth @history next-pos
                                                      [nil @state])]
                            (when (< next-pos (count @history))
                              (reset! state target-state)))}
      ">"]
     [:button {:type :button
               :on-click #(reset! state (second (last @history)))}
      "Reset"]]))
