(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as game]
            [taoensso.timbre :as timbre :refer-macros
             (log trace debug info warn error fatal spy)]))

(enable-console-print!)

;; was a defonce
(def app-state (r/atom (game/game-board 2 :two-ten)))

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


(r/render-component [hexagram] (.getElementById js/document "root"))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

