(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]
            [garden.core :refer [css]]))

(enable-console-print!)

(def debug-mode false)

(defonce app
  (->>
   (g/game-board 2 :two-ten)
   (r/atom)))

(defn ->hex
  "Return the hex value of the color denoted by the keyword."
  [kw]
  (kw {:green ["#95CBB4" "#539C7D" "#002616"]
       :yellow ["#FFF9BA" "#F0E14B" "#575011"]
       :red ["#FFA990" "#DD7354" "#6E1B03"]
       :black ["#717171" "#242424" "#444444"]
       :purple ["#D8B6DF" "#AA76B5" "#571B64"]
       :blue ["#9FAABE" "#4D6BA7" "#122D63"]
       :white ["#FFFFFF" "#E2E2E2" "#A3A39F"]
       :possible-move ["#FD5F00" "#FD5F00" "#FD5F00"]}))

(defn- background-css
  [color]
  (let [[a b c] (->hex color)]
    [{:background b}
     {:background (str "-webkit-radial-gradient(circle, " a ", " b ", " c ")")}
     {:background (str "-o-radial-gradient(circle, " a ", " b ", " c ")")}
     {:background (str "-moz-radial-gradient(circle, " a ", " b ", " c ")")}
     {:background (str "radial-gradient(circle, " a ", " b ", " c ")")}]))

(def backgrounds
  (css (vec (flatten [:.green (background-css :green)]))
       (vec (flatten [:.red (background-css :red)]))
       (vec (flatten [:.yellow (background-css :yellow)]))
       (vec (flatten [:.black (background-css :black)]))
       (vec (flatten [:.purple (background-css :purple)]))
       (vec (flatten [:.blue (background-css :blue)]))
       (vec (flatten [:.white (background-css :white)]))
       (vec (flatten [:.possible-move (background-css :possible-move)]))))

(defn assoc-style
  "Compute the style for this space and return the space with style assoced."
  [space game row col]
  (let [color (g/marble-color game row col)
        color (when color (name color))
        selected? (= [row col] (:selected game))
        possible-move? (contains? (:possible-moves game) [row col])]
    (cond
      ;; currently selected
      selected?
      (assoc space :class (str "space selected glow " color))
      
      ;; a possible move
      possible-move?
      (assoc space :class (str "space possible possible-move"))
      
      ;; owned by current player
      (g/has-marble? @app (g/whose-turn @app) row col)
      (assoc space :class (str "space owned hvr-glow " color))
      
      ;; has an assigned color (it is in the game)
      color
      (assoc space :class (str "space " color))
      
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
    (swap! app merge (g/comp-do-move @app 3))))

;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;

(defn console []
  [:div.console "Turn: " (name (g/whose-turn-color @app))])

(defn space [r c]
  ^{:key [r c]}
  [:div (assoc-style {:class "space"
                      :on-click #(do (mark-selected! r c)
                                     (maybe-move! r c))}
                     @app r c)
   [:style backgrounds]
   [:div
    (when debug-mode
      (str r ", " c))]])


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
     :on-click #(swap! app merge (g/comp-do-move @app 3))}]

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
