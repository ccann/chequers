(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [chequers.ai :as ai]
            [taoensso.timbre :refer-macros (log  trace  debug  info  warn  error  fatal)]
            [garden.core :refer [css]]))

(enable-console-print!)

(def debug-mode true)

(defonce app (r/atom (g/game-board 2 :two-ten)))

(defonce disp (r/atom {:possible-moves #{}}))


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
  (->> [[:.green (background-css :green)]
        [:.red (background-css :red)]              
        [:.yellow (background-css :yellow)]
        [:.black (background-css :black)]
        [:.purple (background-css :purple)]
        [:.blue (background-css :blue)]
        [:.white (background-css :white)]
        [:.possible-move (background-css :possible-move)]]
       (map flatten)
       (map vec)
       (apply css)))

(defn assoc-style
  "Compute the style for this space and return the space with style assoced."
  [space game row col]
  (let [color (g/marble-color game row col)
        color (when color (name color))]
    (cond
      ;; currently selected
      (= [row col] (:selected @disp))
      (assoc space :class (str "space selected glow " color))
      
      ;; a possible move
      (contains? (:possible-moves @disp) [row col])
      (assoc space :class "space possible possible-move")
      
      ;; owned by current player
      (g/has-marble? game (g/whose-turn game) row col)
      (assoc space :class (str "space owned hvr-glow " color))
      
      ;; has an assigned color (it is in the game)
      color
      (if (= [row col] (:moved-to @disp))
        (assoc space :class (str "space fade-in " color))
        (assoc space :class (str "space " color)))

      (= [row col] (:moved-from @disp))
      (assoc space :class "space fade-in")
      
      ;; add no style
      :else space)))

(defn mark-selected!
  "Mark the marble at (row, col) as :selected and all spaces reachable from (row, col)
  as :possible-moves"
  [row col]
  (let [curr-player (g/whose-turn @app)]
    (when (g/has-marble? @app curr-player row col)
      (swap! disp assoc :selected [row col])
      (swap! disp assoc :possible-moves (->> (g/moves-from @app row col)
                                             (map second)
                                             (set))))))

(defn maybe-move!
  "Move the :selected marble to (row, col) IFF the clicked marble at (row, col) is a
  possible move; return bool indicating if we moved."
  [row col]
  (when (contains? (:possible-moves @disp) [row col])
    (let [[r1 c1] (:selected @disp)]
      (swap! app merge (g/do-move @app r1 c1 row col))
      (swap! disp assoc
             :selected nil
             :possible-moves #{}
             :moved-to [row col]
             :moved-from [r1 c1])
      (js/clearInterval (:move-interval @disp))
      true)))

(defn comp-do-move!
  []
  (if (g/winner @app)
    (info "player" (g/winner @app) "wins")
    (do 
      (let [[[r1 c1] [r2 c2]] (ai/compute-move @app 3)]
        (mark-selected! r1 c1)
        (maybe-move! r2 c2)))))

;;;;;;;;;;
;; HTML ;;
;;;;;;;;;;

(defn console []
  [:div.console "Turn: " (name (g/whose-turn-color @app))])

(defn space [r c]
  ^{:key [r c]}
  [:div (assoc-style
         {:class "space"
          :on-click #(do (mark-selected! r c)
                         (when (maybe-move! r c)
                           (swap! disp assoc :move-interval
                                  (js/setInterval comp-do-move! 1500))))}
         @app r c)
   [:div (when debug-mode (str r ", " c))]])

(defn debug-buttons []
  (when debug-mode
    [:div 
     [:input
      {:type "button"
       :value "RESET BOARD"
       :on-click #(reset! app (g/game-board 2 :two-ten))}]

     [:input 
      {:type "button"
       :value "COMP DO MOVE"
       :on-click comp-do-move!}]
     
     [:input
      {:type "button"
       :value "BATTLE OF THE BOTS"
       :on-click #(swap! disp assoc :move-interval
                         (js/setInterval comp-do-move! 3000))}]

     [:input
      {:type "button"
       :value "CEASE FIRE"
       :on-click #(js/clearInterval (:move-interval @disp))}]]))


(defn hexagram []
  [:div
   [:style backgrounds]
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
   [debug-buttons]])

(r/render-component [hexagram] (. js/document (getElementById "app")))



(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
#_(let [game (g/game-board 2 :two-ten)
                                  temp (get-in game [:players 0])
                                  game (assoc-in game [:players 0] (get-in game [:players 3]))
                                  game (assoc-in game [:players 3] temp)
                                  game (g/do-move game 13 5 12 5)
                                  game (g/do-move game 3 6 4 5)
      [[r1 c1] [r2 c2]] (ai/compute-move game 0)]
  ;; [[r1 c1] [r2 c2]])
  
  (g/winner (g/do-move game r1 c1 r2 c2)))

  

