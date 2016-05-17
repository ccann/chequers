(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as game]))

(enable-console-print!)

(println "This text is printed from src/chequers/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app (r/atom (game/game-board 2 :two-ten)))

(defn color->hex
  [kw]
  (kw {:blue (name :blue)
       :green (name :green)
       :black (name :black)
       :white (name :white)
       :red (name :red)
       :yellow (name :yellow)
       :selected "black"
       :highlighted nil}))

(defn space [r c]
  [:div.cell
   (let [m {:style nil
            :on-click
            #(reset! app-state (assoc-in @app-state [:space-states [r c]] :selected))}
         m (game/space-style @app-state color->hex r c m)]
     (info m)
     m)])


     
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


(r/render-component [hexagram] (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
