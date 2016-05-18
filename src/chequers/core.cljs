(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]))

(enable-console-print!)

(def debug-mode true)

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

(defn assoc-style
  "Compute the style for this space and return the space with style assoced."
  [space game row col]
  (let [color (g/marble-color game row col)
        selected? (= [row col] (:selected game))
        possible-move? (contains? (:possible-moves game) [row col])
        background-color [:style :background-color]
        border-color [:style :border-color]]
    ;; (debug row col)
    ;; (debug (map second (:possible-moves game)))
    (cond
      selected?
      (-> space
          (assoc :class "space selected")
          (assoc-in border-color (color->hex :selected))
          (assoc-in background-color (color->hex color)))
      ;; this marb is a possible move
      possible-move?
      (-> space
          (assoc-in background-color (color->hex :possible-move))
          (assoc :class "space possible"))
      ;; this marb has an assigned color (it is in the game)
      color
      (assoc-in space background-color (color->hex color))
      ;; add no style
      :else space)))

(defn mark-selected!
  "Mark the marble at (row, col) as :selected and all spaces reachable from (row, col)
  as :possible-moves"
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
  "Move the :selected marble to (row, col) IFF the clicked marble at (row, col) is a
  possible move. "
  [row col]
  (when (contains? (:possible-moves @app) [row col])
    (let [[r1 c1] (:selected @app)]
      (swap! app merge (g/do-move @app r1 c1 row col))
      (swap! app assoc :selected nil :possible-moves #{}))))

(defn bot-battle
  []
  (when-not (g/winner @app)
    (swap! app merge (g/comp-do-move @app 1))))

;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;

(defn console []
  [:div.console "Turn: " (name (g/whose-turn-color @app))])

(defn space [r c]
  ^{:key [r c]}
  [:div 
   (-> {:class "space"
        :on-click #(do (mark-selected! r c)
                       (maybe-move! r c))}
       (assoc-style @app r c))
   (when debug-mode
     (str r ", " c))])


(defn hexagram []
  [:div
   [:div.table
    [:div
     (doall (for [r (range 17)]
              ^{:key r}
              [:div.row
               (doall (for [c (get g/star r)]
                        (space r c)))]))]]
   [:br]
   [:br]
   [console]
   [:br]
   [:input
    {:type "button"
     :value "RESET BOARD"
     :on-click #(reset! app (g/game-board 2 :two-ten))}]

   [:input 
    {:type "button"
     :value "COMP DO MOVE"
     :on-click #(swap! app merge (g/comp-do-move @app 1))}]

   [:input
    {:type "button"
     :value "BATTLE OF THE BOTS"
     :on-click #(def interval (js/setInterval bot-battle 3000))}]

   [:input
    {:type "button"
     :value "END BLOODSHED"
     :on-click #(js/clearInterval interval)}]

   

])

(r/render-component [hexagram] (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
