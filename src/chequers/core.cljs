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
             :possible-moves (as-> @app $
                                 (g/moves-from $ row col)
                                 (map second $)
                                 (set $))))))

(defn maybe-move!
  [r2 c2]
  (when (contains? (:possible-moves @app) [r2 c2])
    (let [[r1 c1] (:selected @app)]
      (swap! app merge (g/do-move @app r1 c1 r2 c2))
      (swap! app assoc
             :selected nil
             :possible-moves #{}))))

(defn space [r c]
  ^{:key [r c]}
  [:div (-> {:class "space"
             :on-click #(do (mark-selected! r c)
                            (maybe-move! r c))}
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
