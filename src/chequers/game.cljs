(ns chequers.game
  (:require [taoensso.timbre :refer-macros (log trace debug info warn error fatal spy)]))


;;;;;;;;;;;;;
;; Players ;;
;;;;;;;;;;;;;

(def player-count 2)

(def colors #{:blue :green :black :white :red :yellow})
(def players [0 3 1 4 2 5])

(defn- player-colors
  "Return a vec of player (int) and color keyword pairs."
  [n]
  (let [cs (take n (shuffle chequers.game/colors))]
    (zipmap chequers.game/players cs)))

(defn- opponent
  "Return the int representation of this player's opponent."
  [player]
  (-> player (+ 3) (mod 6)))


;;;;;;;;;;;
;; Board ;;
;;;;;;;;;;;

;; (def illegal-spaces {0  [0 1 2 3 4 5   7 8 9 10 11 12],
;;                      1  [0 1 2 3 4 5     8 9 10 11 12],
;;                      2  [0 1 2 3 4       8 9 10 11 12],
;;                      3  [0 1 2 3 4   X     9 10 11 12],
;;                      4  [          Y Y               ],
;;                      5  [0         Z Z Z             ],
;;                      6  [0                         12],
;;                      7  [0 1                       12],
;;                      8  [0 1                    11 12],
;;                      9  [0 1                       12],
;;                      10 [0                         12],
;;                      11 [0                           ],
;;                      12 [                            ],
;;                      13 [0 1 2 3 4         9 10 11 12],
;;                      14 [0 1 2 3 4       8 9 10 11 12],
;;                      15 [0 1 2 3 4 5     8 9 10 11 12],
;;                      16 [0 1 2 3 4 5   7 8 9 10 11 12]})

(def star
  {0 (range 6 7)
   1 (range 6 8)
   2 (range 5 8)
   3 (range 5 9)
   4 (range 0 13)
   5 (range 1 13)
   6 (range 1 12)
   7 (range 2 12)
   8 (range 2 11)
   9 (range 2 12)
   10 (range 1 12)
   11 (range 1 13)
   12 (range 0 13)
   13 (range 5 9)
   14 (range 5 8)
   15 (range 6 8)
   16 (range 6 7)})


;; 1 {4 (range 9 13)
;;    5 (range 10 13)
;;    6 (range 10 12)
;;    7 (range 11 12)}
;; 5 {4 (range 0 4)
;;    5 (range 1 4)
;;    6 (range 1 3)
;;    7 (range 2 3)}
;; 2 {9 (range 11 12)
;;    10 (range 10 12)
;;    11 (range 10 13)
;;    12 (range 9 13)}
;; 4 {9 (range 2 3)
;;    10 (range 1 3)
;;    11 (range 1 4)
;;    12 (range 0 4)}


(def star-corners
  {:two-ten
   {0 {0 (range 6 7)
       1 (range 6 8)
       2 (range 5 8)
       3 (range 5 9)}
    3 {13 (range 5 9)
       14 (range 5 8)
       15 (range 6 8)
       16 (range 6 7)}}
   :two-fifteen
   {0 {0 (range 6 7)
       1 (range 6 8)
       2 (range 5 8)
       3 (range 5 9)
       4 (range 4 9)}
    3 {12 (range 4 9)
       13 (range 5 9)
       14 (range 5 8)
       15 (range 6 8)
       16 (range 6 7)}}})

(defn- opp-star-corner
  "Return a map of the coordinates of the opposite star corner for this player."
  [game-type player]
  (get-in star-corners [game-type (opponent player)]))

(defn game-board
  "Return a starting game-board for n players."
  [n game-type]
  (let [players (take n chequers.game/players)
        colors-map (player-colors n)
        g (as-> players $
            (reduce #(assoc-in %1 [:players %2]
                               (get-in star-corners [game-type %2])) {} $)
            (assoc $
                   :colors colors-map
                   :game-type game-type
                   :turn-seq players
                   :selected nil
                   :possible-moves #{}))]
    (info g)
    g))

;;;;;;;;;;;;;;;;;;;;
;; Game Inquiries ;;
;;;;;;;;;;;;;;;;;;;;

(defn- won
  "Return int of player who has won, or nil if no player has won."
  [game player]
  (let [opp (opp-star-corner (:game-type game) player)
        plr ((:players game) player)]
    (when (= opp plr) player)))

(defn winner
  "Return the winner of the game."
  [game]
  (let [ws (->> game
                (:turn-seq)
                (map #(won game %))
                (remove nil?)
                (seq))]
    (if ws (first ws) nil)))



(defn whose-turn [game] (first (:turn-seq game)))
(defn whose-turn-color [game] (get-in game [:colors (whose-turn game)]))
(defn- rotate-seq [n s] (lazy-cat (drop n s) (take n s)))
(defn next-turn [game] (update-in game [:turn-seq] #(rotate-seq 1 %)))

(defn ->pair [[k vs]] (for [v vs] [k v]))

(defn marble-locs [player game]
  "Return vector of row,col pairs for the locations of current player's marbles."
  (let [marbles (get-in game [:players player])]
    (mapcat ->pair marbles)))

(defn occupied-space?
  "Is the space denoted by row, col occupied?"
  [game row col]
  (let [ms (map val (:players game))
        result (remove false? (for [m ms] (contains? (set (get m row)) col)))]
    (not (empty? result))))

(defn- legal-space?
  "Is this coordinate a legal board space?"
  [row col]
  (contains? (set (star row)) col))

(defn has-marble?
  "Does player have a marble at row,col?"
  [game player row col]
  (contains? (set (marble-locs player game))
             [row col]))

(defn marble-color
  "Return the color of marble at row,col, nil if there isn't one."
  [game row col]
  (let [players (:turn-seq game)
        player (first (for [p players :when (has-marble? game p row col)] p))]
    (get (:colors game) player)))


(defn assoc-style
  "Compute the style for this space and return the space with style assoced."
  [space game row col ->hex]
  (let [color (marble-color game row col)
        selected? (= [row col] (:selected game))
        possible-moves (->> game (:possible-moves) (map second) (set))
        possible-move? (contains? possible-moves [row col])
        background-color [:style :background-color]
        box-shadow [:style :box-shadow]
        border-color [:style :border-color]]
    ;; (debug row col)
    ;; (debug (map second (:possible-moves game)))
    (cond
      selected?
      (-> space
          (assoc-in border-color (->hex :selected))
          (assoc-in box-shadow "inset 0px 0px 0px 3px")
          (assoc-in background-color (->hex color)))
      ;; this marb is a possible move
      possible-move?
      (assoc-in space background-color (->hex :possible-move))
      ;; this marb has an assigned color (it is in the game)
      color
      (assoc-in space background-color (->hex color))
      ;; add no style
      :else space)))

;;;;;;;;;;;;;;
;; Movement ;;
;;;;;;;;;;;;;;


(defn- adjacent-spaces
  "Return all spaces reachable via a single step as a vec of row-col pairs."
  [row col]
  (let [ev (even? row)
        pairs [[row (dec col)]
               [row (inc col)]
               [(dec row) (if ev col (dec col))]
               [(dec row) (if ev (inc col) col)]
               [(inc row) (if ev col (dec col))]
               [(inc row) (if ev (inc col) col)]]]
    pairs))

(defn single-step-moves
  "Return all legal and unoccupied spaces reachable by a step as a vector of row col pairs."
  [game row col]
  (->> (adjacent-spaces row col)
       (filter #(apply legal-space? %))
       (remove #(apply occupied-space? game %))))


(defn move
  "Return game with current player's marble at (r1, c1) moved to (r2, c2)."
  [game r1 c1 r2 c2]
  (cond (not (and (legal-space? r2 c2) (legal-space? r1 c1)))
        (do (error "Illegal move!" r1 c1 "to" r2 c2) game)
        (occupied-space? game r2 c2 )
        (do (error r2 c2 "is occupied, can't move into it!") game)
        (not (occupied-space? game r1 c1))
        (do (error r1 c1 "is not occupied, can't move out of it!") game)
        :else
        (let [plyr (whose-turn game)]
          (-> game
              (update-in [:players plyr r1] #(remove #{c1} %))
              (update-in [:players plyr] #(into {} (filter (comp not empty? val) %))) ;; remove empties
              (update-in [:players plyr r2] conj c2)))))

(defn do-move
  "Move current player's marble and toggle turn."
  [game r1 c1 r2 c2]
  (let [[i c] [(whose-turn game) (whose-turn-color game)]]
    (println "DO MOVE player" i c "from" r1 c1 "to" r2 c2)
    (-> game
        (move r1 c1 r2 c2)
        (next-turn))))
  

(defn- single-hop-spaces
  "Return all spaces reachable via a single hop as a vector of row-col pairs.
  Note: Does not check for adjacent marbles to hope over, just computes coordinates reachable via one hop."
  [row col]
  (let [rs [row       row       (- row 2) (- row 2) (+ row 2) (+ row 2)]
        cs [(- col 2) (+ col 2) (dec col) (inc col) (dec col) (inc col)]
        pairs (map vector rs cs)]
    pairs))

;; (single-hops (game-board 2 :two-ten) 2 6)

(defn single-hop-moves
  "Return all moves via a single hop from this space as a vector of row col pairs."
  [game row col]
  (let [neighbors (adjacent-spaces row col)
        one-hops (single-hop-spaces row col)
        pairs (map vector neighbors one-hops)]
    (for [[neighbor hop] pairs
          :when (and
                 (apply legal-space? neighbor)
                 (apply legal-space? hop)
                 (apply occupied-space? game neighbor)
                 (not (apply occupied-space? game hop)))]
      hop)))


(defn- consecutive-hops
  [game [row col] visited]
  (let [destinations (set (single-hop-moves game row col))
        to-visit (set (remove #(contains? (set visited) %) destinations))
        new-visited (clojure.set/union destinations visited)]
    (if (empty? to-visit)
      visited
      (set (mapcat #(consecutive-hops (apply move game row col %) % new-visited)
                   to-visit)))))

(defn hop-moves
  "Return all spaces reachable via consecutive hops from this position."
  [game row col]
  (let [pairs (consecutive-hops game [row col] #{[row col]})]
    (remove #{[row col]} pairs)))

(defn moves-from
  "Return the vec of moves from this position by curr player."
  [game row col]
  (let [all-moves (concat (single-step-moves game row col)
                          (hop-moves game row col))]
    (for [resulting-space all-moves]
      [[row col] resulting-space])))

(defn all-moves
  "Return vec of all possible moves for the curr player."
  [game]
  (->> game
       (marble-locs (whose-turn game))
       (map #(apply moves-from game %))
       (apply concat)))



;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(defn- find-children
  "Return all descendant-states of this game-state by making all possible moves."
  [game]
  (let [moves (all-moves game)]
    (map (fn [[[r1 c1] [r2 c2]]] (do-move game r1 c1 r2 c2))
         moves)))

;; (find-children (game-board 2 :two-ten))

(defn- euclidean-distance
  [[x1 y1] [x2 y2]]
  (Math/pow
   (+ (Math/pow (- x1 x2) 2)
      (Math/pow (- y1 y2) 2))
   0.5))

;; (euclidean-distance [1 1] [1 3])

(defn score-by-euclidean-distance
  "Return score of curr player's marble locations. Find the euclidean distance between each marble
  and the opposite corner, add them up, and normalize."
  [game]
  (let [marbs (marble-locs (whose-turn game) game)
        corner (opp-star-corner (:game-type game) (whose-turn game))
        location (first (for [[k v] (seq corner)
                              :when (= (count v) 1)]
                          [k (first v)]))
        distances (map #(euclidean-distance % location) marbs)]
    (->> distances 
         (reduce +)
         (/ 1)
         (* 1000))))

;; (time (negamax (game-board 2 :two-ten) 1))
;; (time (negamax (do-move (game-board 2 :two-ten) 2 5 4 4) 1))

(declare descend-tree)

(defn negamax
  "Return the negamax of this node for player denoted by color."
  ([node color] (negamax node color 4 (.-MIN_VALUE js/Number.) (.-MAX_VALUE js/Number.)))
  ([node color depth alpha beta]
   (if (or (= depth 0) (winner node))
     (* color (score-by-euclidean-distance node))
     (->> node
          (find-children)
          (descend-tree depth alpha beta color)
          (remove nil?)
          (cons alpha)
          (apply max)))))

(defn- descend-tree
  "Return a list containing the negamax of each child-node."
  [depth alpha beta color children]
  (loop [cs children
         alpha alpha
         vs []]
    (if-not (seq cs) ;; base case, return values
      vs 
      (let [v (- (negamax (first cs) (- color) (dec depth) (- beta) (- alpha)))
            new-alpha (max alpha v)]
        (if (< new-alpha beta)
          (recur (next cs) new-alpha (conj vs v))
          vs)))))
