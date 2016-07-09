(ns chequers.core
  (:require [reagent.core :as r]
            [chequers.game :as g]
            [chequers.ai :as ai]
            [taoensso.timbre :as t :refer-macros (log trace spy debug info warn error fatal)]
            [garden.core :refer [css]]
            [clojure.string :as str]))

(enable-console-print!)

(def debug-mode false)

(if debug-mode
  (t/set-level! :debug)
  (t/set-level! :info))

(defonce disp (r/atom {:game-triangles-count 2
                       :game-marbles-count 10
                       :turn-marble "pulse"}))

(defn new-game []
  (assoc
   (g/game-board (:game-triangles-count @disp)
                 (:game-marbles-count @disp))
   :game-ai-level 1))

(defonce app (r/atom (new-game)))

(defn ->hex
  "Return the hex value of the color denoted by the keyword."
  [kw]
  (kw {:green ["#95CBB4" "#539C7D" "#002616"]
       :yellow ["#FFF9BA" "#F0E14B" "#575011"]
       :red ["#FFA990" "#DD7354" "#6E1B03"]
       :black ["#717171" "#242424" "#444444"]
       :purple ["#D8B6DF" "#AA76B5" "#571B64"]
       :blue ["#9FAABE" "#4D6BA7" "#122D63"]
       :white ["#FFFFFF" "#E2E2E2" "#A3A39F"]}))

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
        [:.white (background-css :white)]]
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
      (assoc space :class (str "space owned glow " color))
      
      ;; a possible move
      (contains? (:possible-moves @disp) [row col])
      (assoc space :class "space possible hvr-glow")
      
      ;; owned by current player
      (g/has-marble? game (g/whose-turn game) row col)
      (assoc space :class (str "space owned " color))
      
      ;; has an assigned color (it is in the game)
      color
      (if (= [row col] (:moved-to @disp))
        (assoc space :class (str "space fade-in " color))
        (assoc space :class (str "space " color)))

      (= [row col] (:moved-from @disp))
      (assoc space :class "space fade-in")
      
      ;; add no style
      :else space)))

(defn- rotate-turn-marble!
  []
  (do
    (swap! disp assoc :turn-marble "wobble-horizontal")
    (js/setTimeout #(swap! disp assoc :turn-marble "pulse") 1100)))

(defn mark-selected!
  "Mark the marble at (row, col) as :selected and all spaces reachable from (row, col)
  as :possible-moves"
  [row col]
  (let [curr-player (g/whose-turn @app)]
    (when (g/has-marble? @app curr-player row col)
      (swap! disp assoc 
             :selected [row col]
             :possible-moves (->> (g/moves-from @app row col)
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
      (rotate-turn-marble!)
      true)))

(defn toggle-menu!
  "Toggle the menu hide/display"
  []
  (let [curr (:menu-link @disp)]
    (if (= curr "active")
      (swap! disp assoc :menu-link nil)
      (swap! disp assoc :menu-link "active"))))

(defn comp-do-move!
  []
  (if (g/winner @app)
    (info "player" (g/winner @app) "wins")
    (let [[[r1 c1] [r2 c2]] (ai/compute-move @app (:game-ai-level @app))]
      (debug r1 c1)
      (js/setTimeout #(mark-selected! r1 c1) 100)
      (js/setTimeout #(maybe-move! r2 c2) 100))))

(defn player-do-move!
  [r c]
  (mark-selected! r c)
  (when (maybe-move! r c)
    (js/setTimeout comp-do-move! 1200)))

;;;;;;;;;;;;;;;;;;;;;
;; HTML COMPONENTS ;;
;;;;;;;;;;;;;;;;;;;;;

(defn curr-turn
  "Return a component marble denoting current turn."
  []
  (let [curr-color (get (:colors @app) (g/whose-turn @app))
         state (:turn-marble @disp)
         cl (->> curr-color name (str state " "))]
    [:div {:id "turn-marble" :class cl}]))

(defn space
  "Return a component board space"
  [r c]
  ^{:key [r c]}
  [:div (assoc-style
         {:class "space"
          :on-click #(player-do-move! r c)
          :on-mouse-over #(debug [r c])}
         @app r c)])

(defn debug-buttons
  []
  (when debug-mode
    [:div 
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


(defn new-game-config
  []
  [:form {:class "pure-form pure-form-stacked"}
   [:br]
   [:label {:for "game-type"} "corners x marbles"]
   (-> [:select
        {:id "game-type"
         :on-change #(do (swap! disp assoc :game-triangles-count
                                (-> % .-target .-value (str/split #" x ") first js/parseInt))
                         (swap! disp assoc :game-marbles-count
                                (-> % .-target .-value (str/split #" x ") second js/parseInt)))}]
       (concat (mapv #(vector :option %) ["2 x 10" "2 x 15" "4 x 10" "6 x 10"]))
       (vec))
   [:label {:for "ai-level"} "AI level"]
   (-> [:select
        {:id "ai-level"
         :on-change #(swap! app assoc :game-ai-level (-> % .-target .-value js/parseInt))}]
       (concat (mapv #(vector :option %) [1 2 3]))
       (vec))])



(defn sidebar-menu
  "Return component sidebar menu."
  []
  [:div
   {:id "menu" :class (:menu-link @disp)}
   [:div.pure-menu
    [:a.pure-menu-heading {:href "https://github.com/ccann/chequers"} "ccann/chequers"]
     [:ul.pure-menu-list
      [:li.pure-menu-item
       [new-game-config]]
      [:li.pure-menu-item
       [:input {:id "new-game"
                :class "pure-button"
                :type "button"
                :value "New Game"
                :on-click #(reset! app (new-game))}]]]]])

(defn page
  "Return component page, the root of the HTML."
  []
  [:div {:id "layout" :class (:menu-link @disp)}
   [:a.menu-link {:id "menu-link"
                  :class (:menu-link @disp)
                  :href "#menu"
                  :on-click toggle-menu!} [:span]]
   [sidebar-menu]
   [:div.page
    [:style backgrounds]
    [:div.hexagram
     (doall (for [r (range 17)]
              ^{:key r}
              [:div.row
               (doall (for [c (get g/star r)]
                        (space r c)))]))]
    [curr-turn]
    [debug-buttons]]])

(r/render-component [page] (. js/document (getElementById "app")))


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

  

