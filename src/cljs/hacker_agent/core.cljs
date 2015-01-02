(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close! merge]]
            [hacker-agent.hacker-base :as base]
            [hacker-agent.time :as t]
            [hacker-agent.utils :as utils])
  (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {}))

(defonce stories-synced
  (base/bind! app-state [:top-stories] base/top-stories
              (base/stories-binder
               (base/item-cache-fn app-state [:cache]))))

;; ------------------------
;; Components
(defn nav []
  [:div.nav
   [:p "Hacker News"]
   [:ul
    [:li [:a {:href "#/"} "Top"]]
    [:li [:a {:href "#"} "New"]]
    [:li [:a {:href "#"} "Threads"]]
    [:li [:a {:href "#"} "Comments"]]
    [:li [:a {:href "#"} "Show"]]]])

(declare comment-list)
(defn comment [data]
  (let [collapse? (atom false)]
    (fn [data]
      (let [{:keys [id by kids parent text type time score deleted]} @data]
        (when-not deleted
          (if (not id)
            [:p "Loading..."]
            [:div.comment
             [:div.comhead
              [:span {:on-click #(swap! collapse? not)
                      :style {:cursor :pointer}}
               (if @collapse?
                 "[+] "
                 "[-] ")]
              (str by " | " (t/time-ago time) " | ")
              [:a {:href (str "/#/items/" id)} "link"]]
             (when-not @collapse?
               [:div
                [:p {:dangerouslySetInnerHTML {:__html text}}]
                [:span
                 (when kids
                   [comment-list (r/wrap kids swap! data assoc :kids)])]])
             ]))))))

(defn comment-list [comments]
  [:ul.comments
   (for [[id sub-data] (vec @comments)]
     ^{:key id} [:li [comment (r/wrap sub-data
                                      swap! comments assoc id)]])])

(defn story [data]
  (let [{:keys [id by title kids type time url score]} @data]
    (when id
      [:div.top-item
       [:p.title [:a {:href url} title]]
       [:p.subtext
        (str score " points by " by)
        " | "
        [:a {:href (str "/#/items/" id)}
         (str (count kids) " threads")]]
       (when kids
         [comment-list (r/wrap kids swap! data assoc :kids)])])))

(defn story-list-item-fn [story]
  (let [{:keys [by id title score url kids preview -updated?]} @story]
    (when (every? identity [by title score])
      [:div {:class (if -updated?
                      (do
                        (swap! story assoc :-updated? false)
                        "new")
                      "old")}
       [:p.title [:a {:href url} title]]
       [:p.subtext
        (str score " points by " by)
        " | "
        [:a {:href (str "/#/items/" id)}
         (str (count kids) " threads")]
        " | "
        (if preview
          [:span {:on-click #(binding [base/closer-root [:story-list-item id]]
                               (base/close-channel! story [])
                               (swap! story dissoc :preview))
                  :style {:cursor :pointer}}
           [:i "Close"]]
          [:span {:on-click #(binding [base/closer-root [:story-list-item id]]
                               (base/bind! story [:preview]
                                           (base/id->fbref id)
                                           base/item-binder))
                  :style {:cursor :pointer}}
           [:i "Preview"]])]
       (when preview
         [comment-list (r/wrap (preview :kids)
                               swap! story assoc-in [:preview :kids])])])))

(def story-list-item
  (with-meta story-list-item-fn
    {:component-will-unmount #(let [story (get (.. % -props -argv) 1)
                                    id (@story :id)]
                                (binding [base/closer-root [:story-list-item id]]
                                  (base/unbind! story [:preview])))}))

(defn top-stories [stories items]
  [:ol.stories
   (for [[index entry] (into (sorted-map-by #(let [keyfn (comp js/parseInt name)]
                                               (compare (keyfn %1)
                                                        (keyfn %2))))
                             @stories)]
     ^{:key entry}
     [:li
      [story-list-item
           (r/wrap (get @items entry)
                   swap! items assoc entry)]])])

;; -------------------------
;; Views
(defmulti page #(:render-view (deref %)))

(defmethod page nil [_]
  [:div "Invalid/Unknown route"])

(defmethod page :main [state]
  [top-stories
   (r/wrap (:top-stories @state)
           swap! app-state assoc :top-stories)
   (r/wrap (:cache @state)
           swap! app-state assoc :cache)])

(defmethod page :item [state]
  (when-let [entry (get-in @state [:current-item])]
    (case (:type entry)
      "story" [story (r/wrap entry swap! state assoc :current-item)]
      "comment" [comment (r/wrap entry swap! state assoc :current-item)]
      [:p "Cannot render item"])))

(defn main-page [state]
  [:div.main
   [nav]
   [:div.page
    (page state)]])

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (swap! app-state assoc
         :render-view :main)
  (base/unbind! app-state [:current-item]))

(secretary/defroute "/items/:id" [id]
  (swap! app-state assoc :render-view :item)
  (base/unbind! app-state [:current-item])
  (base/bind! app-state [:current-item]
              (base/id->fbref id)
              base/item-binder))

;; -------------------------
;; Initialize app
(defn init! []
  (r/render-component [main-page app-state] (.getElementById js/document "app")))

;; -------------------------
;; History
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))
;; need to run this after routes have been defined
(defonce hooked
  (hook-browser-navigation!))
