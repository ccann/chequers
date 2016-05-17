(ns chequers.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [chequers.game :refer
             [game-board whose-turn next-turn single-step-moves winner
              occupied-space? move]]))

(defn set= [& vectors] (apply = (map set vectors)))


(deftest test-movement
  (let [g (game-board 2 :two-ten)
        g2 (assoc-in g [:players 0] {13 (range 5 9)
                                       14 (range 5 8)
                                       15 (range 6 8)
                                       16 (range 6 7)})]

    (testing "game board and inquiries...")
    (is (= {0 {0 '(6)
               1 '(6 7)
               2 '(5 6 7)
               3 '(5 6 7 8)}
            3 {13 '(5 6 7 8)
               14 '(5 6 7)
               15 '(6 7)
               16 '(6)}}
           (:players g)))
    (is (= '(0 3) (:turn-seq g)))
    (is (= 2 (count (:colors g))))
    (is (= :two-ten (:game-type g)))

    (is (= 0 (whose-turn g)))
    (is (= 3 (whose-turn (next-turn g))))
    (is (= 0 (whose-turn (next-turn (next-turn g)))))

    (is (= nil (winner g)))
    (is (= 0 (winner g2)))

    (is (true? (occupied-space? g 0 6)))
    (is (false? (occupied-space? g 0 7)))

    (testing "single step moves...")
    (is (= [[4 7] [4 9] [5 8] [5 9]]
           (single-step-moves g 4 8)))
    (is (= [[5 6] [5 8] [4 6] [4 7] [6 6] [6 7]]
           (single-step-moves g 5 7)))
    (is (= []
           (single-step-moves g 0 1)))

    (is (= {1 '(6 7)
            2 '(5 6 7)
            3 '(5 6 7 8)
            4 '(6)}
           (get-in (move g 0 6 4 6) [:players 0])))


    ))

(run-tests)
