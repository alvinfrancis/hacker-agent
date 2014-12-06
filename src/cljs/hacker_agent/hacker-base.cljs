(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [pani.cljs.core :as p]))

(defonce hacker-root "https://hacker-news.firebaseio.com/v0")

(defn- id->fbref [id]
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

(defn- id->fb-chan [id]
  (fb->chan (id->fbref id)))

(defn init-data-sync!
  "Given vector PATH and atom DATA, create callbacks on channel
  created from item ID to update PATH in DATA.  Channels created to
  update DATA will be stored in atom CHANS as a hash-map of ID and
  chan."
  ([data id]
   (init-data-sync! [:item] data id))
  ([path data id]
   (init-data-sync! path data id [:channels]))
  ([path data id chan-path]
   (let [fbc (id->fb-chan id)]
     (swap! data assoc-in (conj chan-path id) fbc)
     (letfn [(handle-kid [k v child-path]
               (let [items (doall (map #(hash-map % {}) v))]
                 (swap! data assoc-in child-path (into {} items))
                 (doseq [id v]
                   (init-data-sync! (conj child-path id) data id chan-path))))]
       (go-loop []
         (when-let [msg (<! fbc)]
           (let [[event key val] msg
                 child-path (conj path key)]
             (cond 
              (= event :child_added)
              (if (= key :kids)
                (handle-kid key val child-path)
                (swap! data assoc-in child-path val))

              (= event :child_changed)
              (if (= key :kids)
                (handle-kid key val child-path)
                (swap! data assoc-in child-path val))
              ;; TODO: implement dissoc-in?
              (= event :child_removed)
              (swap! data assoc-in child-path nil) 

              :else (.log js/console (clj->js [event key val]))
              ))
           (recur)))))))

(defn reset-data-sync! [data id]
  (swap! data
         #(do
            (when-let [{id-chans :channels} %]
              (doseq [[id chan] (seq id-chans)]
                (.log js/console (str "Closing channel " id "."))
                (close! chan)))
            (dissoc % :item :channels)))
  (init-data-sync! data id))
