(ns chequers.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

;; was a defonce
(def app-state (r/atom {:text "huh???!"}))

(defn hello-world []
  [:h1 (:text @app-state)])



(defn hole [] [:td])
(defn row [n] [:tr (repeat n [hole])])

(defn hexagram []
  [:table (map #(conj %2 %1)
               [1 2 3 4 13 12 11 10 9 10 11 12 13 4 3 2 1]
               (repeat 17 [row]))])




(r/render-component [hexagram] (.getElementById js/document "root"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
