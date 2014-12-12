(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge mult tap]]
            [pani.cljs.core :as p]))

(defonce url "https://hacker-news.firebaseio.com/v0")

(defonce root (js/Firebase. url))

(defonce channels (atom {}))

(defn- walk-root [r keys]
  (let [[k & ks] keys]
    (if ks
      (recur (.child r (clojure.core/name k)) ks)
      (.child r (clojure.core/name k)))))

(defn- unbind!
  ([fb event keys]
   (.off (walk-root fb keys) (clojure.core/name event)))
  ([fb event keys f]
   (.off (walk-root fb keys) (clojure.core/name event) f)))

(defn- bind!
  ([fb event keys f]
   (.on (walk-root fb keys)
        (clojure.core/name event)
        f)))

(defn bind-item!
  [id data path]
  (bind! root :value [:item (str id)]
         (fn [snapshot]
           (let [db (js->clj (.val snapshot) :keywordize-keys true)]
             (swap! data assoc-in path db)))))

(defn bind-top-stories! [data path]
  (bind! root :value [:topstories]
         (fn [snapshot]
           (let [db (js->clj (.val snapshot))
                 stories (map #(hash-map % {}) db)]
             (swap! data assoc-in path db)
))))

(defn- id->fbref [id]
  (walk-root root [:item (str id)]))

(defn- fb->chan
  ([fbref]
   (fb->chan fbref (chan)))
  ([fbref close-chan]
   (let [events [:child_added :child_changed :child_removed]
         event-chans (map (fn [event]
                            (fb->chan fbref close-chan event))
                          events)]
     (merge event-chans)))
  ([fbref close-chan event]
   (let [event-chan (chan)
         handle-event (fn [snapshot]
                        (put! event-chan
                              [event
                               (keyword (.key snapshot))
                               (js->clj (.val snapshot))]))]
     (.on fbref (clojure.core/name event) handle-event)
     (go (<! close-chan)
         (.off fbref (clojure.core/name event)
               handle-event))
     event-chan)))

(defn- id->fb-chan
  ([id] (id->fb-chan id (chan)))
  ([id close-chan]
   (fb->chan (id->fbref id) close-chan)))

(defn id->atom
  ([id] (id->atom (atom {}) id))
  ([data id] (id->atom data id []))
  ([data id path]
   (let [fbc (id->fb-chan id)]
     (go-loop []
       (when-let [msg (<! fbc)]
         (let [[event key val] msg
               child-path (conj path key)]
           (case event 
             :child_added (swap! data assoc-in child-path val)
             :child_changed (swap! data assoc-in child-path val)
             :child_removed (swap! data update-in path dissoc key) 
             (.log js/console (clj->js [event key val]))))
         (recur)))
     data)))

(defn init-item-sync!
  "Given vector PATH and atom DATA, create callbacks on channel
  created from item ID to update PATH in DATA.  Returns a channel that
  accepts any input to remove the callbacks created."
  ([data id] (init-item-sync! data id [:entry]))
  ([data id path]
   (let [close-chan (chan)]
     (init-item-sync! data id (conj path :item) (mult close-chan))
     close-chan))
  ([data id path close-mult]
   ;; mult to multiplex reading the close channel
   ;; tap to create a channel from the mult
   (let [fbc (id->fb-chan id (tap close-mult (chan)))
         close-loop-tap (tap close-mult (chan))]
     (letfn [(handle-kid [k v child-path]
               (let [items (doall (map #(hash-map % {}) v))]
                 (swap! data assoc-in child-path (into {} items))
                 (doseq [id v]
                   (init-item-sync! data id (conj child-path id) close-mult))))]
       ;; spawn a go loop that will update the data
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
               :child_removed (swap! data update-in path dissoc key)
               (.log js/console (clj->js [event key val]))))
           (recur)))
       ;; The go loop still processes messages even after using .off
       ;; on the source. As a workaround for now, create another tap
       ;; on the close-mult and manually close the channel here.
       (go (<! close-loop-tap)
           (close! fbc))
       close-mult))))

(defn reset-item-sync! [id data path]
  (let [identifier (cons data path)]
    (when-let [close-chan (get-in @channels identifier)]
      (put! close-chan :close))
    (swap! data update-in path dissoc :item)
    (swap! channels assoc-in identifier
           (init-item-sync! data id path))))
