(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]))

(enable-console-print!)

(println "This text is printed from src/chequers/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app (r/atom (g/game-board 2 :two-ten)))

(defn color->hex
  [kw]
  (kw {:blue (name :blue)
       :green (name :green)
       :black (name :black)
       :white (name :white)
       :red (name :red)
       :yellow (name :yellow)
       :selected "white"
       :highlighted nil}))

(defn mark-as-selected!
  "Mark the marble at (row, col) as selected if no marbles are currently marked."
  [row col]
  (debug (vals  (:space-states @app)))
  (debug (some #(= % :selected) (-> @app :space-states vals)))
  (swap! app assoc-in [:space-states [row col]]
         (if (some #(= % :selected) (-> @app :space-states vals))
           nil
           :selected)))


(defn space [r c]
  (let [m (-> {:style {:box-shadow "inset 0px 0px 0px 0px"}
               :on-click #(mark-as-selected! r c)}
              (g/assoc-style @app r c color->hex))]
    [:div.cell m]))

     
(defn row [r]
  [:div.row
   (let [col-range (get g/star r)
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
