(ns hacker-agent.utils)

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
