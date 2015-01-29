(ns hacker-agent.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.set :as set]
            [secretary.core :as secretary :include-macros true]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [weasel.repl :as ws-repl]
            [cljs.core.async :as async :refer [put! chan <! >! close!]]
            [hacker-agent.hacker-base :as base]
            [hacker-agent.time :as t]
            [hacker-agent.utils :as utils]
            [hacker-agent.debug :as debug])
  (:import goog.History))

;; -------------------------
;; State
(defonce app-state (atom {}))

(defonce stories-synced
  (base/bind! app-state [:top-stories] base/top-stories
              base/stories-binder))

(defn stream-binder [f]
  (fn [data path msg]
    (let [[event key val] msg
          child-path (conj path key)
          add-change-fn #(do
                           (f val)
                           (swap! data assoc-in child-path val))]
      (case event
        :value (add-change-fn)
        (.log js/console (clj->js [event key val]))))))

(defonce new-synced
  (base/bind! app-state [:stream] base/max-item
              (stream-binder
               (base/item-cache-fn app-state [:stream :items]))))
;; -------------------------
;; Debug
(defonce debug? (atom false))

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
    [:li [:a {:href "#"} "Show"]]
    [:li [:a {:href "#/stream"} "Stream"]]]])

(def ticker
  (r/create-class
   {:component-will-receive-props (fn [this new-props]
                                    (let [old-val (:value (r/props this))
                                          new-val (:value (get new-props 1))]
                                      (when-not (= old-val new-val)
                                        (r/set-state this {:updated? true}))))
    :component-did-update (fn [this old-props old-children]
                            (when (:updated? (r/state this))
                              (r/set-state this {:updated? false})))
    :render (fn [this]
              [:div.nav
               {:style (if (:updated? (r/state this))
                         {:position :absolute
                          :top :-25px}
                         {:top :-25px
                          :position :absolute
                          :-webkit-animation  "slidein 3s"
                          :animation "slidein 3s"})}
               [:p (:value (r/props this))]])}))

(declare comment-list)
(defn comment [data & top?]
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
              [:a {:href (str "/#/items/" id)} "link"]
              (when top?
                [:span
                 " | "
                 [:a {:href (str "/#items/" parent)} "Parent"]])]
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

(def anim-score
  (r/create-class
   {:component-will-receive-props (fn [this new-props]
                                    (let [old-val (:value (r/props this))
                                          new-val (:value (get new-props 1))]
                                      (when-not (= old-val new-val)
                                        (r/set-state this {:updated? true}))))
    :component-did-update (fn [this old-props old-children]
                            (when (:updated? (r/state this))
                              (r/set-state this {:updated? false})))
    :render (fn [this]
              [:span {:class (if (:updated? (r/state this))
                               "new"
                               "old")}
               (:value (r/props this))])}))

(defn story-title [title url]
  [:p.title [:a {:href url} title]
   (when-not (empty? url)
     [:span.comhead (str " (" (utils/domain url) ") ")])])

(defn story [data]
  (let [{:keys [id by title kids type time url score]} @data]
    (when id
      [:div.top-item
       [story-title title url]
       [:p.subtext
        [anim-score {:value score}]
        (str " points by " by)
        " | "
        [:a {:href (str "/#/items/" id)}
         (str (count kids) " threads")]]
       (when kids
         [comment-list (r/wrap kids swap! data assoc :kids)])])))

(defn story-list-item-fn [story]
  (let [{:keys [by id title score url kids preview]} @story]
    (when (every? identity [by title score])
      [:li.story-list-item
       [:div
        [story-title title url]
        [:p.subtext
         [anim-score {:value score}]
         (str " points by " by)
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
                                            (base/item-binder :levels 1)))
                   :style {:cursor :pointer}}
            [:i "Preview"]])]
        (when preview
          [comment-list (r/wrap (preview :kids)
                                swap! story assoc-in [:preview :kids])])]])))

(def story-list-item
  (with-meta story-list-item-fn
    {:component-will-unmount #(let [story (get (.. % -props -argv) 1)]
                                (when-let [id (:id @story)]
                                  (binding [base/closer-root [:story-list-item id]]
                                    (base/unbind! story [:preview]))))}))

(defn top-stories [stories]
  [:ol.stories
   (for [[index entry] (into (sorted-map-by #(let [keyfn (comp js/parseInt name)]
                                               (compare (keyfn %1)
                                                        (keyfn %2))))
                             (:list @stories))]
     ^{:key entry}
     [story-list-item
      (r/wrap (-> @stories :items (get entry))
              swap! stories assoc-in [:items entry])])])

(defn stream [items]
  [:ul
   (for [[id entry] (reverse @items)]
     ^{:key id}
     (when-not (:deleted entry)
       [:li
        (case (:type entry)
          "story" [story (r/wrap entry swap! assoc items id)]
          "comment" [comment (r/wrap entry swap! assoc items id) true]
          [:p "Cannot render item"])]))])

;; -------------------------
;; Views
(defmulti page #(:render-view (deref %)))

(defmethod page nil [_]
  [:div "Invalid/Unknown route"])

(defmethod page :main [state]
  [top-stories
   (r/wrap (:top-stories @state)
           swap! app-state assoc :top-stories)])

(defmethod page :item [state]
  (when-let [entry (get-in @state [:current-item])]
    (case (:type entry)
      "story" [story (r/wrap entry swap! state assoc :current-item)]
      "comment" [comment (r/wrap entry swap! state assoc :current-item) true]
      [:p "Cannot render item"])))

(defmethod page :stream [state]
  (when-let [entries (get-in @state [:stream :items])]
    [stream (r/wrap entries swap! assoc-in state [:stream :items])]))

(defn debug [state]
  (when @debug?
    [debug/console state]))

(defn main-page [state]
  [:div.main
   [debug state]
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
              (base/item-binder)))

(secretary/defroute "/stream" []
  (swap! app-state assoc :render-view :stream))

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

;; -------------------------
;; Events
(defonce key-chan (utils/listen (dom/getDocument) (.-KEYPRESS events/EventType)))

(defonce key-loop
  (go-loop []
    (when-let [event (<! key-chan)]
      (when (= (.. event -keyCode) 4) ; CTRL-D
        (swap! debug? not))
      (recur))))
