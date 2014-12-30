(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [clojure.walk :as walk]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge mult tap]]
            [hacker-agent.utils :as utils :refer [dissoc-in]]))

(defonce url "https://hacker-news.firebaseio.com/v0")

(defonce root (js/Firebase. url))

(defn- walk-root [r keys]
  (let [[k & ks] keys]
    (if ks
      (recur (.child r (clojure.core/name k)) ks)
      (.child r (clojure.core/name k)))))

(def top-stories (walk-root root [:topstories]))

(def ^:dynamic closer-root nil)

(defonce channel-closers (atom {}))

(defonce type-chan?
  (let [type-chan (type (chan))]
    (fn [x] (= type-chan (type x)))))

(defn- save-channel-closer! [data path channel]
  (let [c-root (if closer-root closer-root data)]
    (swap! channel-closers assoc-in
           (conj (vec (cons c-root path)) :-closer)
           channel)))

(defn- close-channel! [data path]
  (let [c-root (if closer-root closer-root data)
        identifier (cons c-root path)]
    (when-let [closers (get-in @channel-closers identifier)]
      (walk/postwalk (fn [c]
                       (when (type-chan? c)
                         (put! c :close)))
                     closers)
      (swap! channel-closers
             #(let [new-closers (dissoc-in % identifier)]
                (if (empty? (new-closers c-root))
                  (dissoc % c-root)
                  new-closers))))))

(defn- id->fbref [id]
  (walk-root root [:item (str id)]))

(defn- fb->chan
  ([fbref]
   (fb->chan fbref (chan)))
  ([fbref close-chan]
   (let [mult-close (mult close-chan)
         events [:child_added :child_changed :child_removed]
         event-chans (map (fn [event]
                            (fb->chan fbref (tap mult-close (chan)) event))
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
         (.off fbref (clojure.core/name event) handle-event))
     event-chan)))

(defn- id->fb-chan
  ([id] (id->fb-chan id (chan)))
  ([id close-chan]
   (fb->chan (id->fbref id) close-chan)))

(declare bind!)

(defn story-binder [data path msg]
  (swap! data assoc-in (conj path :-updated?) true)
  (let [[event key val] msg
        child-path (conj path key)]
    (case event
      :child_added (swap! data assoc-in child-path val)
      :child_changed (swap! data assoc-in child-path val)
      :child_removed (swap! data update-in path dissoc key)
      (.log js/console (clj->js [event key val])))))

(defn stories-binder [data path msg]
  (let [[event key val] msg
        child-path (conj path key)]
    (case event
      :child_added (bind! data child-path
                          (id->fbref val)
                          story-binder)
      :child_changed (bind! data child-path
                            (id->fbref val)
                            story-binder)
      (.log js/console (clj->js [event key val])))))

(defn item-binder [data path msg]
  (let [[event key val] msg
        child-path (conj path key)]
    (case event
      :child_added (if (= key :kids)
                     (do
                       (swap! data
                              (fn [d]
                                (apply update-in d child-path assoc
                                       (reduce #(into %1 [%2 {}])
                                               [] val))))
                       (doseq [id val]
                         (bind! data (conj child-path id)
                                (id->fbref id)
                                item-binder)))
                     (swap! data assoc-in child-path val))
      :child_changed (if (= key :kids)
                       (do
                         (swap! data
                                (fn [d]
                                  (apply update-in d child-path assoc
                                         (reduce #(into %1 [%2 {}])
                                                 [] val))))
                         (doseq [id val]
                           (bind! data (conj child-path id)
                                  (id->fbref id)
                                  item-binder)))
                       (swap! data assoc-in child-path val))
      :child_removed (swap! data update-in path dissoc key)
      (.log js/console (clj->js [event key val])))) )

(defn unbind! [data path]
  (close-channel! data path)
  (swap! data dissoc-in path))

(defn bind! [data path ref binder]
  (close-channel! data path)
  (let [close-chan (chan)
        fbc (fb->chan ref close-chan)
        cr closer-root
        bind-fn (fn [data path msg]
                  (binding [closer-root cr]
                    (binder data path msg)))]
    (save-channel-closer! data path close-chan)
    (go-loop []
      (when-let [msg (<! fbc)]
        (bind-fn data path msg)
        (recur)))
    close-chan))
