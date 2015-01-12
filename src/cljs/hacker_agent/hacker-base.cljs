(ns hacker-agent.hacker-base
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [clojure.walk :as walk]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge mult tap]]
            [hacker-agent.utils :as utils :refer [dissoc-in]]))

(defonce url "https://hacker-news.firebaseio.com/v0")

(defonce root (js/Firebase. url))

(defn- walk-root [r & keys]
  (let [[k & ks] keys]
    (if ks
      (recur (.child r (clojure.core/name k)) ks)
      (.child r (clojure.core/name k)))))

(def top-stories (walk-root root :topstories))

(def max-item (walk-root root :maxitem))

(def ^:dynamic closer-root nil)

(defonce channel-closers (atom {}))

(defonce cache-store (atom []))

(defonce cache-limit 2000)

(defn cache-ref-off [url ref]
  (let [cached? (some (fn [[-url _]]
                        (= -url url))
                      @cache-store)]
    (when-not cached?
      (.on ref "value" identity)
      (swap! cache-store
             (fn [store]
               (let [over-limit (- (count store) cache-limit)
                     new-store (conj store [url #(.off ref "value" identity)])]
                 (if (> over-limit 0)
                   (do
                     (doseq [[_ f] (take over-limit new-store)]
                       (f))
                     (into [] (drop over-limit new-store)))
                   new-store)))))))

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
  (walk-root root :item (str id)))

(defn- fb->chan
  ([fbref]
   (fb->chan fbref (chan)))
  ([fbref close-chan]
   (let [mult-close (mult close-chan)
         events [:child_added :child_changed :child_removed :value]
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
         (.off fbref (clojure.core/name event) handle-event)
         (close! event-chan))
     event-chan)))

(defn- id->fb-chan
  ([id] (id->fb-chan id (chan)))
  ([id close-chan]
   (fb->chan (id->fbref id) close-chan)))

(declare bind! unbind!)

(defn story-binder [data path msg]
  (let [[event key val] msg
        child-path (conj path key)]
    (case event
      :child_added (swap! data assoc-in child-path val)
      :child_changed (swap! data assoc-in child-path val)
      :child_removed (swap! data update-in path dissoc key)
      :value nil
      (.log js/console (clj->js [event key val])))))

(defn item-cache-fn
  ([data path]
   (item-cache-fn data path 0))
  ([data path limit]
   (fn [id]
     (let [cached (sort (get-in @data path))
           child-path (conj path id)
           in-cached? (get cached id)
           over-limit (- (count cached) limit)]
       (when-not in-cached?
         (when (and (> limit 0)
                    (>= over-limit 0))
           (doseq [id (->> cached
                           (take (inc over-limit))
                           (map key))]
             (unbind! data (conj path id))))
         (swap! data assoc-in child-path {})
         (bind! data child-path
                (id->fbref id)
                story-binder)))
     id)))

(defn stories-binder [f]
  (fn [data path msg]
    (let [[event key val] msg
          child-path (conj path key)]
      (case event
        :child_added (do
                       (f val)
                       (swap! data assoc-in child-path val))
        :child_changed (do
                         (f val)
                         (swap! data assoc-in child-path val))
        :value nil
        (.log js/console (clj->js [event key val]))))))

(defn r-item-binder [f]
  (fn [data path msg]
    (let [[event key val] msg
          child-path (conj path key)
          add-change-fn #(do
                           (swap! data assoc-in child-path val)
                           (when (= key :kids)
                             (doseq [id val]
                               (f id))))]
      (case event
        :child_added (add-change-fn)
        :child_changed (add-change-fn)
        :child_removed (swap! data update-in path dissoc key)
        :value nil
        (.log js/console (clj->js [event key val]))))))

(defn- r-cache-fn [data path]
  (fn [id] ; id is a str or number
    (let [child-path (conj path (long id))]
      (when-not (get-in @data child-path)
        (bind! data child-path
               (id->fbref id)
               (r-item-binder (r-cache-fn data path)))))))

(defn r-cache! [data path id]
  ((r-cache-fn data path) id))

(defn custom-binder [& {:keys [add-fn change-fn remove-fn value-fn]
                        :or {add-fn identity
                             change-fn identity
                             remove-fn identity
                             value-fn identity}}]
  (fn [data path msg]
    (let [[event key val] msg
          child-path (conj path key)]
      (case event
        :child_added (add-fn data path msg)
        :child_changed (change-fn data path msg)
        :child_removed (remove-fn data path msg)
        :value (value-fn data path msg)
        (.log js/console (clj->js [event key val]))))))

(defn item-binder [& {:keys [levels]}]
  (let [add-change-fn (fn [data path [event key val]]
                        (let [child-path (conj path key)]
                          (if (= key :kids)
                            (when (or (> levels 0) (not levels))
                              (swap! data
                                     (fn [d]
                                       (apply update-in d child-path assoc
                                              (reduce #(into %1 [%2 {}])
                                                      [] val))))
                              (doseq [id val]
                                (bind! data (conj child-path id)
                                       (id->fbref id)
                                       (item-binder :levels (and levels (dec levels))))))
                            (swap! data assoc-in child-path val))))]
    (custom-binder :add-fn add-change-fn
                   :change-fn add-change-fn)))

(defn unbind! [data path]
  (close-channel! data path)
  (swap! data dissoc-in path))

(defn bind! [data path ref binder]
  (cache-ref-off (.toString ref) ref)
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
