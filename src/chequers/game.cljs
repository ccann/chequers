(ns chequers.game
  (:require [taoensso.timbre :as timbre :refer-macros (trace debug info warn error fatal)]))

(def player-count 2)

(def ^:private colors #{:blue :green :black :white :red :yellow})

(defn- assign-rand-colors
  "Assign to n players a random color where n <= 6."
  [colors plyr-seq n]
  (let [cs (take n (shuffle colors))]
    (zipmap plyr-seq cs)))


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

;; 3, 6 -> 5, 7 or 5, 5

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

(defn- opponent [player] (-> player (+ 3) (mod 6)))
(defn- opp-star-corner [game-type player] (get-in star-corners [game-type (opponent player)]))

(defn- won
  [game player]
  (let [opp (opp-star-corner (:game-type game) player)
        plr ((:players game) player)]
    (when (= opp plr) player)))


(defn winner
  "Return a seq of the winners of the game."
  [game]
  (let [winners (remove nil? (map #(won game %) (:turn-seq game)))]
    (seq winners)))



(defn mk-game-board
  "Return a starting game-board for this many players."
  [player-count game-type]
  (let [plyr-seq (take player-count [0 3 1 4 2 5])
        plyr-colors (assign-rand-colors colors plyr-seq player-count)]
    (as-> plyr-seq $
      (take player-count $)
      (reduce #(assoc-in %1 [:players %2] (get-in star-corners [game-type %2])) {} $)
      (assoc $ :colors plyr-colors)
      (assoc $ :game-type game-type)
      (assoc $ :turn-seq plyr-seq))))

(defn- rotate [n s] 
  (lazy-cat (drop n s) 
            (take n s)))


(defn whose-turn [game] (first (:turn-seq game)))
(defn whose-turn-color [game] (get-in game [:colors (whose-turn game)]))
(defn next-turn [game] (update-in game [:turn-seq] #(rotate 1 %)))



;;;;;;;;;;;;;;
;; Movement ;;
;;;;;;;;;;;;;;

(defn- legal-space?
  "True if this space is a legal board space, false otherwise."
  [row col]
  (contains? (set (star row)) col))

(defn steps-naive
  "Return all spaces reachable via a single step as a vector of row-col pairs."
  [row col]
  (let [ev (even? row)
        pairs [[row (dec col)]
               [row (inc col)]
               [(dec row) (if ev col (dec col))]
               [(dec row) (if ev (inc col) col)]
               [(inc row) (if ev col (dec col))]
               [(inc row) (if ev (inc col) col)]]]
    pairs))

(defn occupied-space?
  "Return true if space denoted by row, col is occupied, False otherwise."
  [game row col]
  (let [ms (map val (:players game))
        result (remove false? (for [m ms] (contains? (set (get m row)) col)))]
    (not (empty? result))))

(defn move
  "Move current players marble at r1, c1 to r2 c2, return new game."
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


(defn single-hops-naive
  "Return all spaces reachable via a single hop as a vector of row-col pairs.
  Note: Does not check for adjacent marbles to hope over, just computes hypothetical hop
  coordinates."
  [row col]
  (let [rs [row       row       (- row 2) (- row 2) (+ row 2) (+ row 2)]
        cs [(- col 2) (+ col 2) (dec col) (inc col) (dec col) (inc col)]
        pairs (map vector rs cs)]
    pairs))

;; (single-hops (mk-game-board 2 :two-ten) 2 6)

(defn single-hops
  "Return all legal and unoccupied spaces reachable via a single hop as a vector of row col pairs."
  [game row col]
  (let [ss (steps-naive row col)
        hs (single-hops-naive row col)
        pairs (map vector ss hs)]
    ;; (prn ss)
    ;; (prn hs)
    (for [[step hop] pairs
          :when (and
                 (apply legal-space? step)
                 (apply legal-space? hop)
                 (apply occupied-space? game step)
                 (not (apply occupied-space? game hop)))]
      hop)))

(defn steps
  "Return all legal and unoccupied spaces reachable by a step as a vector of row col pairs."
  [game row col]
  (->> (steps-naive row col)
       (filter #(apply legal-space? %))
       (remove #(apply occupied-space? game %))))

;; (steps (mk-game-board 2 :two-ten) 13 6)

(defn- hops-helper
  [game [row col] visited]
  (let [destinations (set (single-hops game row col))
        to-visit (set (remove #(contains? (set visited) %) destinations))
        new-visited (clojure.set/union destinations visited)]
    (if (empty? to-visit)
      visited
      (set (mapcat #(hops-helper (apply move game row col %) % new-visited)
                   to-visit)))))

(defn hops
  "Return all legal and unoccupied spaces hoppable-to from this position."
  [game row col]
  (let [pairs (hops-helper game [row col] #{[row col]})]
    (remove #{[row col]} pairs)))

;; (hops (mk-game-board 2 :two-ten) 2 6)


(defn- moves-from
  [game row col]
  (let [ss (steps game row col)
        hs (hops game row col)
        moves (concat ss hs)]
    (for [pair moves] [[row col] pair])))

;; (moves-from (mk-game-board 2 :two-ten) 2 6)

(defn- ->pair [[k vs]] (for [v vs] [k v]))

(defn marble-locs [game]
  "Return vector of row,col pairs for the locations of current player's marbles."
  (let [plyr (whose-turn game)
        marbles (get-in game [:players plyr])]
    (mapcat ->pair marbles)))

(defn all-moves
  "Return seq of all available moves for the current player."
  [game]
  (->> game
       (marble-locs)
       (map #(apply moves-from game %)) 
       (apply concat)))

;; (all-moves (mk-game-board 2 :two-ten))

