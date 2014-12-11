(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [pani.cljs.core :as p]))

(defonce url "https://hacker-news.firebaseio.com/v0")

(defonce root (js/Firebase. url))


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
         close-chan (chan)
         handle-event (fn [snapshot]
                        (put! event-chan
                              [event
                               (keyword (.key snapshot))
                               (js->clj (.val snapshot))]))]
     (.on fbref (clojure.core/name event) handle-event)
     (go
       (let [msg (<! close-chan)]
         (.off fbref (clojure.core/name event)
               handle-event)))
     event-chan)))

(defn- id->fb-chan [id]
  (fb->chan (id->fbref id)))

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
  created from item ID to update PATH in DATA.  Channels created to
  update DATA will be stored in atom CHANS as a hash-map of ID and
  chan."
  ([data id] (init-item-sync! data id [:entry]))
  ([data id path] (init-item-sync! data id (conj path :item) (conj path :channels)))
  ([data id path chan-path]
   (let [fbc (id->fb-chan id)]
     (swap! data assoc-in (conj chan-path id) fbc)
     (letfn [(handle-kid [k v child-path]
               (let [items (doall (map #(hash-map % {}) v))]
                 (swap! data assoc-in child-path (into {} items))
                 (doseq [id v]
                   (init-item-sync! data id (conj child-path id) chan-path))))]
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

(defn reset-item-sync! [id data path]
  (swap! data
         #(do
            (when-let [id-chans (get-in % (conj path :channels))]
              (doseq [[id chan] (seq id-chans)]
                (close! chan)))
            (update-in %
                       path
                       dissoc :item :channels)))
  (init-item-sync! data id path))
