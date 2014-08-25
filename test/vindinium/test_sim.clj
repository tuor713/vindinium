(ns vindinium.test-sim
  (:require [vindinium.model :as m]
            [vindinium.simulation :as sim])
  (:use clojure.test))

(def sample-state
  {:game {:id "a2uvsc6n", :turn 0, :maxTurns 12, 
          :heroes {1 {:name "malvert", :pos [0 2], :crashed false, :life 100, :gold 0, :id 1, 
                      :spawnPos [0 2], :elo 1246, :userId "07zfa8jx", :mineCount 0}
                   
                   2 {:id 2, :name "random", :pos [9 2], :life 100, :gold 0, :mineCount 0, 
                      :spawnPos [9 2], :crashed false} 
                   3 {:id 3, :name "random", :pos [9 7], :life 100, :gold 0, :mineCount 0, 
                      :spawnPos [9 7], :crashed false} 
                   4 {:id 4, :name "random", :pos [0 7], :life 100, :gold 0, :mineCount 0, 
                      :spawnPos [0 7], :crashed false}}, 
          :board [[[:mine nil] :empty [:hero 1] :tavern :wall :wall :tavern [:hero 4] :empty [:mine nil]] 
                  [:wall :empty :empty :wall :wall :wall :wall :empty :empty :wall] 
                  [:wall :wall :empty :empty :empty :empty :empty :empty :wall :wall] 
                  [:wall :wall :wall :empty [:mine nil] [:mine nil] :empty :wall :wall :wall] 
                  [:wall :wall :empty :empty :empty :empty :empty :empty :wall :wall] 
                  [:wall :wall :empty :empty :empty :empty :empty :empty :wall :wall] 
                  [:wall :wall :wall :empty [:mine nil] [:mine nil] :empty :wall :wall :wall] 
                  [:wall :wall :empty :empty :empty :empty :empty :empty :wall :wall] 
                  [:wall :empty :empty :wall :wall :wall :wall :empty :empty :wall] 
                  [[:mine nil] :empty [:hero 2] :tavern :wall :wall :tavern [:hero 3] :empty [:mine nil]]],
          :finished false}, 
   :hero {:name "malvert", :pos {:x 0, :y 2}, :crashed false, :life 100, :gold 0, :id 1, 
          :spawnPos {:x 0, :y 2}, :elo 1246, :userId "07zfa8jx", :mineCount 0}, 
   :token "lcjk", 
   :viewUrl "http://vindinium.org/a2uvsc6n", 
   :playUrl "http://vindinium.org/api/a2uvsc6n/lcjk/play"})
(def sample-game (:game sample-state))


(let [g {:heroes {1 {:pos [0 2] :spawnPos [0 0] :life 15 :gold 10 :mineCount 2}
                  2 {:pos [1 1] :spawnPos [2 0] :life 60 :gold 10 :mineCount 0}
                  3 {:pos [2 2] :spawnPos [3 0] :life 1 :gold 10 :mineCount 1}}
         :board [[:empty :empty [:hero 1] :wall]
                 [[:mine nil] [:hero 2] [:mine 1] :empty]
                 [:empty :tavern [:hero 3] :empty]
                 [[:mine 1] :empty :empty [:mine 3]]]}]

  (deftest test-simulation
    (testing "Illegal moves"
      (is (nil? (sim/step g 1 :north))
          "Moving out of the map")
      (is (nil? (sim/step g 1 :east))
          "Moving into a wall"))

    (let [g' (assoc-in g [:heroes 3 :spawnPos] [0 2])
          g# (sim/step g' 3 :north)]
      (testing "spawning kill"
        (is (= [0 2] (m/pos g# 3)))
        (is (= [0 0] (m/pos g# 1)))
        (is (= 2 (m/mine-count g# 3)))
        (is (= 0 (m/mine-count g# 1)))
        (is (= [:mine 3] (m/tile (m/board g#) [3 0])))))

    (testing "Go into empty terrain"
      (let [g' (sim/step g 3 :east)]
        (is (= [2 3] (m/pos g' 3)))
        (is (= 1 (m/life g' 3)))
        (is (= :empty (m/tile (m/board g') [2 2])))
        (is (= [:hero 3] (m/tile (m/board g') [2 3])))))

    (testing "No-op behaviour"
      (let [g' (sim/step g 1 :stay)]
        (is (= [0 2] (m/pos g' 1))
            "Position unchanged")
        (is (= (m/board g) (m/board g'))
            "Board unchanged")
        (is (= 12 (m/gold g' 1))
            "Gold accrues from mines")
        (is (= 14 (m/life g' 1))
            "Thirst decreases life")
        (is (= 1 (m/life (sim/step g 3 :stay) 3))
            "No thirst at life=1")))

    (testing "Mine behaviour: attacking own mine"
      (let [g' (sim/step g 1 :south true)]
        (is (= 14 (m/life g' 1))
            "No life loss")
        (is (= 2 (m/mine-count g' 1))
            "No mine count update")
        (is (= [0 2] (m/pos g' 1))
            "No position change")
        (is (= (m/board g) (m/board g'))
            "No board change")
        (is (nil? (sim/step g 1 :south))
            "Attaching own mine is not a canonical move")))

    (testing "Mine behaviour: conquering mine"
      (let [g' (sim/step g 2 :east)]
        (is (= 39 (m/life g' 2))
            "Life gets hit by 20")
        (is (= 1 (m/mine-count g' 2))
            "Mine count is updated")
        (is (= 11 (m/gold g' 2))
            "Gold starts accruing")
        (is (= (m/pos g 2) (m/pos g' 2))
            "No position change")))

    (testing "Mine behaviour: death"
      (let [g' (sim/step g 3 :north)]
        (is (= 99 (m/life g' 3))
            "Life back up to 100 + thirst")
        (is (= 0 (m/mine-count g' 3))
            "No more mines")
        (is (= (m/gold g 3) (m/gold g' 3))
            "No more gold earned, but no loss")
        (is (= [:mine nil] (m/tile (m/board g') [3 3]))
            "Mines niled out on the board")
        (is (= (m/pos g' 3) (m/spawn-pos g 3))
            "Hero is now on the spawn position")
        (is (= [:hero 3] (m/tile (m/board g') (m/pos g' 3)))
            "Board has hero at start position")
        (is (= :empty (m/tile (m/board g') (m/pos g 3)))
            "Previous position is empty")))

    (testing "Tavern behaviour"
      (let [g' (sim/step g 3 :west)]
        (is (= 50 (m/life g' 3))
            "Life updated by 50 + thirst")
        (is (= (m/pos g' 3) (m/pos g 3))
            "No change in position")
        (is (= 9 (m/gold g' 3))
            "Cost of 2 gold"))
      
      (is (= 99 (m/life (sim/step g 2 :south) 2))
          "Life does not go beyond 100")
      (is (= 59 (m/life (sim/step (assoc-in g [:heroes 2 :gold] 1) 2 :south) 2))
          "No tavern boost with < 2 gold")
      (is (= 1 (m/gold (sim/step (assoc-in g [:heroes 2 :gold] 1) 2 :south) 2))))))