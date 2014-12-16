(ns hacker-agent.time)

(defn- now []
  (quot
   (.. js/Date (now))
   10))

(defn- in-minutes [seconds]
  (quot seconds 60))

(defn- in-hours [seconds]
  (quot (in-minutes seconds) 60))

(defn- in-days [seconds]
  (quot (in-hours seconds) 24))

(defn minutes [x]
  (* 60 x))

(defn hours [x]
  (* (minutes 60) x))

(defn days [x]
  (* (hours 24) x))

(defn time-ago
  [timestamp]
  (let [time-diff (- (now) timestamp)]
    (cond
     ;; seconds
     (< time-diff (minutes 1))
     (let [i time-diff]
       (str i " second" (when-not (= i 1) "s") " ago"))
     ;; minutes
     (< time-diff (hours 1))
     (let [i (in-minutes time-diff)]
       (str i " minute" (when-not (= i 1) "s") " ago"))
     ;; hours
     (< time-diff (days 1))
     (let [i (in-hours time-diff)]
       (str i " hour" (when-not (= i 1) "s") " ago"))
     ;; days
     :else (let [i (in-days time-diff)]
             (str i " day" (when-not (= i 1) "s") " ago")))))
