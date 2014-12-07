(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [pani.cljs.core :as p]))

(defonce url "https://hacker-news.firebaseio.com/v0")

(defonce root (js/Firebase. url))

(defn- bind! [fb event target f]
  (.on (.child fb target)
       event
       f))

(defn bind-top-stories! [data path]
  (bind! root "value" "topstories"
         (fn [snapshot]
           (let [db (js->clj (.val snapshot))]
             (swap! data assoc-in path db)))))

(defn- id->fbref [id]
  (.. root
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
  ([data id] (init-data-sync! data id [:entry]))
  ([data id path] (init-data-sync! data id (conj path :item) (conj path :channels)))
  ([data id path chan-path]
   (let [fbc (id->fb-chan id)]
     (swap! data assoc-in (conj chan-path id) fbc)
     (letfn [(handle-kid [k v child-path]
               (let [items (doall (map #(hash-map % {}) v))]
                 (swap! data assoc-in child-path (into {} items))
                 (doseq [id v]
                   (init-data-sync! data id (conj child-path id) chan-path))))]
       (go-loop []
         (when-let [msg (<! fbc)]
           (let [[event key val] msg
                 child-path (conj path key)]
             (case event 
               :child_added (if (= key :kids)
                              (handle-kid key val child-path)
                              (swap! data assoc-in child-path val))
               :child_changed (if (= key :kids)
                                (handle-kid key val child-path)
                                (swap! data assoc-in child-path val))
               ;; TODO: implement dissoc-in?
               :child_removed (swap! data assoc-in child-path nil) 
               (.log js/console (clj->js [event key val]))))
           (recur)))))))

(defn reset-data-sync! [id data path]
  (swap! data
         #(do
            (when-let [id-chans (get-in % (conj path :channels))]
              (doseq [[id chan] (seq id-chans)]
                (close! chan)))
            (update-in %
                       path
                       dissoc :item :channels)))
  (init-data-sync! data id path))
