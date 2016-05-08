(ns play-with-routing.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [play-with-routing.handlers]
            [play-with-routing.subs]
            [play-with-routing.routes :as routes :refer [Router Route Link]]
            [play-with-routing.views :as views]
            [play-with-routing.config :as config]))

(when config/debug?
  (println "dev mode"))

;; home

(defn root []
  (let [name (re-frame/subscribe [:name])]
    (fn []
      [:div (str "Hello from " @name ". This is the root Page.")
       [:div
        [Link {:display "Go to about page"
               :href "/about"}]]])))

;; error

(defn error-page []
  (fn []
    [:div {:style {:width "100%"
                   :text-align "center"
                   :padding-top 100}}
     [:h1 {:style {:color "red"}} "404 - Error Occurred"]
     [Link {:display "Go to home page"
            :href "/"}]]))

;; about

(defn about-page []
  (fn [props]
    [:div "This is the About Page."
     [:div
      (pr-str props)
      [Link {:display "Go to home page"
             :href "/"}]]]))

(defn mount-root []
  (reagent/render
    [Router {:on-error #(js/console.log "router saw error")
             :on-update #(js/console.log "router updated")}
     [Route {:path "/" :component root}]
     [Route {:path "/about" :component about-page}]
     [Route {:path "*" :component error-page}]]
    (.getElementById js/document "app")))

(defn ^:export init []
  (re-frame/dispatch-sync [:initialize-db])
  (mount-root))
