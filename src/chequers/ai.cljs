(ns chequers.ai 
  (:require [taoensso.timbre :refer-macros (log trace debug info warn error spy)]
            [chequers.game :as g]))

(def max-reward 1000000)

(defn- euclidean-distance
  [[x1 y1] [x2 y2]]
  (Math/pow
   (+ (Math/pow (- x1 x2) 2)
      (Math/pow (- y1 y2) 2))
   0.5))

(defn- find-children
  "Return all descendant-states of this game-state by making all possible moves."
  [game]
  (let [moves (-> game g/all-moves shuffle)]
    (map (fn [[[r1 c1] [r2 c2]]] (g/do-move game r1 c1 r2 c2))
         moves)))

(defn agg-distances
  [game player]
  (let [marb-coords (g/marble-locs player game)
        star-coords (->> player
                         (g/opp-star-corner (:marbs-count game))
                         (mapcat g/->pair))
        open-star-coords (clojure.set/difference (set star-coords) (set marb-coords))
        pairs (map vector open-star-coords
                   (for [sc open-star-coords]
                     (->> marb-coords
                          (map #(euclidean-distance % sc))
                          (reduce +))))
        distance (->> pairs (sort-by second >) first second)]
    distance))

;; (defn score-by-distance
;;   "Return the score of the current game state for this player."
;;   [game player]
;;   (let [player-score (agg-distances game player)
;;         opp-score (agg-distances game (g/opponent player))]
;;     (- (- player-score) (- opp-score))))

(defn score-by-distance
  "Return the score of the current game state for this player."
  [game player]
  (->> player
       (agg-distances game)
       (/ 1)
       (* 1000)))

(declare descend-tree)

(defn negamax
  "Return the negamax of this node for player denoted by color."
  ([node player color]
   (negamax node player color 4 (.-MIN_VALUE js/Number.) (.-MAX_VALUE js/Number.)))
  ([node player color depth]
   (negamax node player color depth (.-MIN_VALUE js/Number.) (.-MAX_VALUE js/Number.)))
  ([node player color depth alpha beta]
   ;; (println "node:" node)
   ;; (println "depth:" depth)
   ;; (println "alpha:" alpha "beta:" beta)
   ;; (debug "node:" node)
   
   (cond
     (= (g/winner node) player)
     (do (debug "WINNER NODE") (* color max-reward))
     (= depth 0)
     (-> node (score-by-distance player) (* color))
     :else
     (->> node
          (find-children)
          (descend-tree player depth alpha beta color)
          (remove nil?)
          (cons alpha)
          (apply max)))))

(defn- descend-tree
  "Return a list containing the negamax of each child-node."
  [player depth α β color children]
  (loop [children children
         α        α
         values   []]
    (if children
      (let [value (- (negamax (first children) player (- color) (dec depth) (- β) (- α)))
            new-α (max α v)]
        (if (< new-α β)
          (recur (next children) new-α (conj vs value))
          values))
      values)))

(defn- third [coll] (nth coll 2))

(defn ->move
  [v player]
  {:player player
   :score  (first v)
   :state  (second v)
   :from   (-> v third first)
   :to     (-> v third second)})

(defn rand-move
  [moves]
  (if (= (-> moves first :state g/winner)
         (-> moves first :player))
    (first moves)
    (let [candidates (take 3 moves)]
      (debug "candidates:" (map #(dissoc % :state) candidates))
      (rand-nth candidates))))

(defn compute-move
  "Return a vector of move-from and move-to coordinate pairs."
  [game depth]
  (let [moves                   (-> game g/all-moves shuffle)
        player                  (g/whose-turn game)
        possible-states         (->> moves
                                     (mapv flatten)
                                     (mapv #(apply g/do-move game %)))
        scores                  (map #(do (negamax % player 1 depth)) possible-states)
        {:keys [score from to]} (->> moves
                                     (map vector scores possible-states)
                                     (sort-by first >)
                                     (map #(->move % player))
                                     (rand-move))]
    (info "scores:" (sort > scores))
    (info "COMP MOVE from" from "to" to "with score" score)
    [from to]))
