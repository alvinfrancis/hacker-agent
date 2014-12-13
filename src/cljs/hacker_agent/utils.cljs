(ns hacker-agent.utils)

(defn dissoc-in [data path]
  (let [prefix (butlast path)
        key (last path)]
    (if prefix
      (update-in data prefix dissoc key)
      (dissoc data key))))
