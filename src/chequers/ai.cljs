(ns chequers.ai 
  (:require [taoensso.timbre :refer-macros (log trace debug info warn error)]
            [chequers.game :as g]))

;; ap: add dpendency
;; fe: function from example
;; am: add a missing libspec



;; (defn negamax2 [node alpha beta color]
;;   (let [node-value (eval-leaf node)]
;;     (if node-value
;;       (* color node-value)
;;       (->> node
;;            (find-children)
;;            (map #(- (negamax2 % (- beta) (- alpha) (- color))))
;;            (take-while #(< % beta))
;;            (cons alpha)
;;            (apply max)))))

;; (time (negamax {:v 0} 1))


;; ok so negamax gives the player the score for the node they are on. Which means that we look at
;; each possible move (there are only a few, hopefully, and we call negamax on that node to get the
;; maximum utility of moving to that place. Then we move to the best one!


;; player A: color = 1



;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(defn- find-children
  "Return all descendant-states of this game-state by making all possible moves."
  [game]
  (let [moves (g/all-moves game)]
    (map (fn [[[r1 c1] [r2 c2]]] (g/do-move game r1 c1 r2 c2))
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
  [game player]
  (let [game-type (:game-type game)
        marb-coords (g/marble-locs player game)
        star-coords (->> player
                         (g/opp-star-corner game-type)
                         (mapcat g/->pair))
        open-star-coords (clojure.set/difference (set star-coords) (set marb-coords))
        pairs (map vector open-star-coords (for [sc open-star-coords]
                                             (->> marb-coords
                                                  (map #(euclidean-distance % sc))
                                                  (reduce +))))
        ;; _ (debug (map #(->> % (second) (/ 1) (* 10000))
        ;;               (sort-by second > pairs)))
        deepest (first (sort-by second > pairs))
        distance (->> deepest (second) (/ 1) (* 1000))]
    ;; (debug "deepest:" deepest)
    ;; (debug "score:" distance)
    distance))

;; (defn score-by-euclidean-distance
;;   "Return score of curr player's marble locations. Find the euclidean distance between each marble
;;   and the opposite corner, add them up, and normalize."
;;   [game player]
;;   (debug "game:" game)
;;   (debug "whose turn:" (whose-turn game))
;;   (let [marbs (marble-locs player game)
;;         corner (opp-star-corner (:game-type game) player)
;;         location (first (for [[k v] (seq corner)
;;                               :when (= (count v) 1)]
;;                           [k (first v)]))
;;         distances (map #(euclidean-distance % location) marbs)
;;         total (->> distances 
;;                    (reduce +)
;;                    (/ 1)
;;                    (* 1000))]
;;     (debug marbs)
;;     (debug location)
;;     (debug total)
;;     total))

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
     (do (debug "WINNER NODE") (* color 1000000))
     (= depth 0)
     (-> node (score-by-euclidean-distance player) (* color))
     :else
     (->> node
          (find-children)
          (descend-tree player depth alpha beta color)
          (remove nil?)
          (cons alpha)
          (apply max)))))

(defn- descend-tree
  "Return a list containing the negamax of each child-node."
  [player depth alpha beta color children]
  (loop [cs children
         alpha alpha
         vs []]
    (if-not (seq cs) ;; base case, return values
      vs 
      (let [v (- (negamax (first cs) player (- color) (dec depth) (- beta) (- alpha)))
            new-alpha (max alpha v)]
        (if (< new-alpha beta)
          (recur (next cs) new-alpha (conj vs v))
          vs)))))

(defn compute-move
  "Return a vector of move-from and move-to coordinate pairs."
  [game depth]
  (let [moves (g/all-moves game)
        player (g/whose-turn game)
        possible-states (->> moves
                             (mapv flatten)
                             (mapv #(apply g/do-move game %)))
        scores (map #(do (negamax % player 1 depth)) possible-states)
        [score [[r1 c1] [r2 c2]]] (->> moves
                                       (map vector scores)
                                       (sort-by first >)
                                       (first))]
    (debug "COMP MOVE from" r1 c1 "to" r2 c2 "with score" score)
    [[r1 c1] [r2 c2]]))
