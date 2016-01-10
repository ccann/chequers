(ns chequers.ai 
  (:require [taoensso.timbre :as timbre
             :refer-macros (log  trace  debug  info  warn  error  fatal)]))

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


