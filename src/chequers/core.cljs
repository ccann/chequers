(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]))

(enable-console-print!)

(def debug-mode false)

(defonce app
  (->>
   (g/game-board 2 :two-ten)
   (r/atom)))

(defn color->hex
  "Return the hex value of the color denoted by the keyword."
  [kw]
  (let [bounds {:red ["red" "yellow" "white"]
                ;; :green ["#002616" "#539C7D" "#95CBB4"]
                :green ["#95CBB4" "#539C7D" "#002616"]
                :white ["white" "white" "white"]
                :black ["black" "black" "black"]
                :yellow ["yellow" "yellow" "yellow"]
                :blue ["blue" "blue" "blue"] 
                :possible-move ["#FD5F00" "#FD5F00" "#FD5F00"]
                :selected ["white" "white" "white"]}]
    (kw bounds)))

(defn assoc-background
  [space color]
  (let [[a b c] (color->hex color)]
    (-> space
        (assoc :style {:background (str "-webkit-radial-gradient(circle, " a ", " b ", " c ")")})
         

;; (str
;;                       ;; b ", "
;;                           "-webkit-radial-gradient(circle, " a ", " b ", " c ")"
;;                           ;; "-o-radial-gradient(circle, " a ", " b ", " c "), "
;;                           ;; "-moz-radial-gradient(circle, " a ", " b ", " c "), "
;;                           ;; "radial-gradient(circle, " a ", " b ", " c ")"
;; )
)))


(defn assoc-style
  "Compute the style for this space and return the space with style assoced."
  [space game row col]
  (let [color (g/marble-color game row col)
        selected? (= [row col] (:selected game))
        possible-move? (contains? (:possible-moves game) [row col])]
    ;; (debug row col)
    ;; (debug (map second (:possible-moves game)))
    (cond
      ;; currently selected
      selected?
      (-> space
          (assoc :class "space selected glow")
          (assoc-background color))
      
      ;; a possible move
      possible-move?
      (-> space
          (assoc-background :possible-move)
          (assoc :class "space possible"))

      ;; owned by current player
      (g/has-marble? @app (g/whose-turn @app) row col)
      (-> space
          (assoc-background color)
          (assoc :class "space owned hvr-glow"))

      ;; has an assigned color (it is in the game)
      color
      (-> space
          (assoc-background color)
          (assoc :class "space"))
      
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
