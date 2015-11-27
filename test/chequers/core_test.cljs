(ns chequers.core-test
  (:require [clojure.test :refer :all]
            [chequers.game :refer :all]))

(defn set= [& vectors] (apply = (map set vectors)))

(deftest test-single-step
  (testing "single step")
  (is (= (single-step 4 8)
         [[4 9] [4 7] [3 8] [5 9] [5 8]]))
  (is (= (single-step 5 7)
         [[5 8] [5 6] [4 6] [4 7] [6 6] [6 7]]))
  (is (= (single-step 0 1)
         [])))


(deftest test-game
  (let [g (mk-game-board 2 :two-ten)]
    (is (= (:players g)
           {0 {0 '(6), 1 '(6 7), 2 '(5 6 7), 3 '(5 6 7 8)}, 3 {13 '(5 6 7 8), 14 '(5 6 7), 15 '(6 7), 16 '(6)}}))
    (is (= (:turn-seq g)
           '(0 3)))
    (is (= (count (:colors g))
           2))
    (is (= (:game-type g) :two-ten))
    (is (= (whose-turn g) 0))
    (is (= (whose-turn (next-turn g)) 3))
    (is (= (whose-turn (next-turn (next-turn g))) 0))))

(deftest test-win
  (let [g1 (mk-game-board 2 :two-ten)
        g2 (assoc-in g1 [:players 0] {13 (range 5 9)
                                      14 (range 5 8)
                                      15 (range 6 8)
                                      16 (range 6 7)})]
    (is (= (winner g1) nil))
    (is (= (winner g2) [0]))))


(deftest test-move
  (let [g (mk-game-board 2 :two-ten)]
    (is (= (get-in (move g 0 6 4 6) [:players 0])
           {1 '(6 7), 2 '(5 6 7), 3 '(5 6 7 8), 4 '(6)}))))

(deftest test-occupied
  (let [g (mk-game-board 2 :two-ten)]
    (is (true? (occupied-space? g 0 6)))
    (is (false? (occupied-space? g 0 7)))))
