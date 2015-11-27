(ns chequers.game
  (:require [taoensso.timbre :as timbre :refer-macros (trace debug info warn error fatal)]))

(def player-count 2)

(def ^:private colors #{:blue :green :black :white :red :yellow})

(defn- assign-rand-colors
  "Assign to n players a random color where n <= 6."
  [colors plyr-seq n]
  (let [cs (take n (shuffle colors))]
    (zipmap plyr-seq cs)))

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


(defn- legal-space?
  "True if this space is a legal board space, false otherwise."
  [row col]
  (contains? (set (star row)) col))

(defn single-step
  "Return all legal spaces reachable via a single step as a vector of row-col pairs."
  [row col]
  (let [op (if (even? row) inc dec)
        rs [row       row       (dec row) (dec row) (inc row) (inc row)]
        cs [(inc col) (dec col) (op col)  col       (op col)   col]
        pairs (map vector rs cs)]
    (filter #(apply legal-space? %) pairs)))

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

(defn occupied-space?
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



