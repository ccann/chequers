(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as game]
            [taoensso.timbre :as timbre :refer-macros
             (log trace debug info warn error fatal spy)]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

;; was a defonce
(def app-state (r/atom (game/game-board 2 :two-ten)))

(defn hello-world []
  [:h1 (:text @app-state)])


(defn space [r c]
  [:div.cell
   {:style {:background-color
            (let [players (:turn-seq @app-state)
                  cs (for [p players
                           :when (contains?
                                  (set (game/marble-locs p @app-state))
                                  [r c])]
                       p)]
              (println (:colors @app-state))
              (cond (= (count cs) 1)
                    (name (get (:colors @app-state) (first cs)))
                    :else nil))}}])

(defn row [r]
  [:div.row
   (let [col-range (get game/star r)
         num-cols (count col-range)]
     (map #(conj %2 %1)
          col-range
          (repeat num-cols [space r])))])

(defn hexagram []
  [:div.table (map #(conj %2 %1)
               (range 17)
               (repeat 17 [row]))])


(r/render-component [hexagram] (.getElementById js/document "root"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

