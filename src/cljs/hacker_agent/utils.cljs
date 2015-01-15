(ns hacker-agent.utils
  (:require [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :as async :refer [put! chan <!]]))

(defn dissoc-in [data path]
  (let [prefix (butlast path)
        key (last path)]
    (if prefix
      (update-in data prefix dissoc key)
      (dissoc data key))))

(defn log-clj [o]
  (.log js/console (clj->js o)))

(defn domain [url]
  (let [re (new js/RegExp "^https?\\:\\/\\/(www\\.)?([^\\/:?#]+)(?:[\\/:?#]|$)")
        matches (.exec re url)]
    (and matches (get matches 2))))

(defn keys-in [m]
  (if (map? m)
    (mapcat (fn [[k v]]
              (let [sub (keys-in v)]
                (conj sub k)))
            m)
    []))

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
      (fn [e] (put! out e)))
    out))
