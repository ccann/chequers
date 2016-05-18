(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]))

(enable-console-print!)

(defonce app
  (->>
   (g/game-board 2 :two-ten)
   (r/atom)))

(defn color->hex
  "Return the hex value of the color denoted by the keyword."
  [kw]
  (kw {:blue (name :blue)
       :green (name :green)
       :black (name :black)
       :white (name :white)
       :red (name :red)
       :yellow (name :yellow)
       :possible-move "#FD5F00"
       :selected "white"}))

(defn mark-selected!
  "Mark the marble at (row, col) as selected if no marbles are currently marked."
  [row col]
  (let [curr-player (g/whose-turn @app)]
    (when (g/has-marble? @app curr-player row col)
      (swap! app assoc
             :selected [row col]
             :possible-moves (set (g/moves-from @app row col))))))

(defn space [r c]
  ^{:key [r c]}
  [:div.cell  (-> {:style {:box-shadow "inset 0px 0px 0px 0px"}
                   :on-click #(mark-selected! r c)}
                  (g/assoc-style @app r c color->hex))])

(defn hexagram []
  [:div.table
   [:div
    (doall (for [r (range 17)]
             ^{:key r}
             [:div.row
              (doall (for [c (get g/star r)]
                       (space r c)))]))]])

   
(r/render-component [hexagram] (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
