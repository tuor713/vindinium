(ns vindinium.test-bots
  (:require [vindinium.model :as m])
  (:use clojure.test
        vindinium.bots))

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

(deftest test-state-accessors
  (is (= [0 2] (state->hero-pos sample-state 1)))
  (is (= [0 2] (state->own-pos sample-state))))




(let [g {:heroes {1 {:pos [0 2] :spawnPos [0 0] :life 15 :gold 10 :mineCount 2}
                  2 {:pos [1 1] :spawnPos [2 0] :life 60 :gold 10 :mineCount 0}
                  3 {:pos [2 2] :spawnPos [3 0] :life 1 :gold 10 :mineCount 1}}
         :board [[:empty :empty [:hero 1] :wall]
                 [[:mine nil] [:hero 2] [:mine 1] :empty]
                 [:empty :tavern [:hero 3] :empty]
                 [[:mine 1] :empty :empty [:mine 3]]]}]

  (deftest test-simulation
    (testing "Illegal moves"
      (is (nil? (move g 1 :north))
          "Moving out of the map")
      (is (nil? (move g 1 :east))
          "Moving into a wall"))

    (testing "No-op behaviour"
      (let [g' (move g 1 :stay)]
        (is (= [0 2] (m/pos g' 1))
            "Position unchanged")
        (is (= (m/board g) (m/board g'))
            "Board unchanged")
        (is (= 12 (m/gold g' 1))
            "Gold accrues from mines")
        (is (= 14 (m/life g' 1))
            "Thirst decreases life")
        (is (= 1 (m/life (move g 3 :stay) 3))
            "No thirst at life=1")))

    (testing "Mine behaviour: attacking own mine"
      (let [g' (move g 1 :south)]
        (is (= 14 (m/life g' 1))
            "No life loss")
        (is (= 2 (m/mine-count g' 1))
            "No mine count update")
        (is (= [0 2] (m/pos g' 1))
            "No position change")
        (is (= (m/board g) (m/board g'))
            "No board change")))

    (testing "Mine behaviour: conquering mine"
      (let [g' (move g 2 :east)]
        (is (= 39 (m/life g' 2))
            "Life gets hit by 20")
        (is (= 1 (m/mine-count g' 2))
            "Mine count is updated")
        (is (= 11 (m/gold g' 2))
            "Gold starts accruing")
        (is (= (m/pos g 2) (m/pos g' 2))
            "No position change")))

    (testing "Mine behaviour: death"
      (let [g' (move g 3 :north)]
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
      (let [g' (move g 3 :west)]
        (is (= 50 (m/life g' 3))
            "Life updated by 50 + thirst")
        (is (= (m/pos g' 3) (m/pos g 3))
            "No change in position")
        (is (= 9 (m/gold g' 3))
            "Cost of 2 gold"))
      
      (is (= 99 (m/life (move g 2 :south) 2))
          "Life does not go beyond 100")
      (is (= 59 (m/life (move (assoc-in g [:heroes 2 :gold] 1) 2 :south) 2))
          "No tavern boost with < 2 gold")
      (is (= 1 (m/gold (move (assoc-in g [:heroes 2 :gold] 1) 2 :south) 2))))


    ))

(deftest test-tavern-finder
  (is (= [[0 0] [0 1] [0 2] [1 2] [2 2]]
         (path-to-tavern 
          [[:empty :empty :empty]
           [:empty :wall :empty]
           [:empty :wall :tavern]]
          [0 0]))))

(deftest test-mine-finder
  (is (= [[0 0] [0 1] [0 2] [1 2] [2 2]]
         (path-to-mine 
          [[:empty :empty :empty]
           [:empty :wall :empty]
           [:empty :wall [:mine nil]]]
          1
          [0 0]))))

(let [g {:heroes {1 {:pos [1 0] :spawnPos [0 0] :life 100 :gold 10 :mineCount 2 :id 1}
                  2 {:pos [1 3] :spawnPos [0 0] :life 100 :gold 10 :mineCount 2 :id 2}
                  3 {:pos [3 3] :spawnPos [0 0] :life 30 :gold 10 :mineCount 0 :id 3}
                  4 {:pos [3 0] :spawnPos [0 0] :life 30 :gold 10 :mineCount 2 :id 4}}
         :board [[:empty :empty :empty :empty]
                 [[:hero 1] :empty :empty [:hero 2]]
                 [:empty :empty :empty :wall]
                 [[:hero 4] :empty :empty [:hero 3]]]}]
  (deftest test-hero-one-oh-one
    (is (= {[0 2] -1 [0 3] -1 [1 1] -1 [1 2] -1 [2 2] -1}
           (enemy-mod-map g 50 (m/hero g 2))))
    (let [g' (assoc-in g [:board 0 1] :tavern)]
      (is (= {[0 2] 0 [0 3] -1 [1 1] 0 [1 2] -1 [2 2] -1}
             (enemy-mod-map g' 50 (m/hero g' 2))))
      (is (= {[0 2] -1 [0 3] -1 [1 1] -1 [1 2] -1 [2 2] -1}
             (enemy-mod-map g 10 (m/hero g 2)))))
    
    (is (= {[3 1] -1 [2 2] -1}
           (enemy-mod-map g 50 (m/hero g 3))))

    (let [g' (assoc-in g [:heroes 3 :life] 15)]
      (is (= {[3 1] -1 [2 2] -1 [3 2] 0.5}
             (enemy-mod-map g' 50 (m/hero g' 3)))))

    (is (= {[2 0] 0.5 [1 0] -1 [3 1] 0.5 [3 2] -1 [2 1] -1}
           (enemy-mod-map g 50 (m/hero g 4))))
    (let [g' (assoc-in g [:board 3 1] :tavern)]
      (is (= {[2 0] -1 [1 0] -1 [2 1] 0}
             (enemy-mod-map g' 50 (m/hero g' 4)))))
    (let [g' (-> (assoc-in g [:board 3 1] :tavern)
                 (assoc-in [:heroes 4 :life] 15))]
      (is (= {[2 0] 0.5 [1 0] -1 [2 1] 0}
             (enemy-mod-map g' 50 (m/hero g' 4)))))

    (is (= {[2 2] -2, [1 0] -1, [1 1] -1, [0 3] 0.5, [0 2] -1, [2 0] 0.5, [3 1] -0.5, [2 1] -1, [1 2] 0.5, [3 2] -1}
           (enemies-mod-map g 1)))
    
    (is (= [[:south 0.5] [:east -1] [:stay -1]]
           (hero-one-oh-one 
            {:game g
             :hero {:id 1}})))
    (is (= [[:south 0.5]]
           (hero-one-oh-one 
            {:game (assoc-in g [:heroes 1 :mineCount] 0)
             :hero {:id 1}})))))

(let [s {:game {:heroes {1 {:pos [1 1] :spawnPos [2 2] :life 100 :gold 10 :mineCount 2 :id 1}
                         2 {:pos [0 0] :spawnPos [1 2] :life 100 :gold 10 :mineCount 2 :id 2}
                         3 {:pos [0 0] :spawnPos [0 0] :life 100 :gold 10 :mineCount 2 :id 2}}
                :board [[:empty :empty :empty]
                        [:empty :empty :empty]
                        [:empty :empty :empty]]}
         :hero {:id 1}}
      sut (avoid-spawning-spots 0.1)]
  (deftest test-avoid-spawning
    (is (= #{[:east -1] [:north -0.1] [:stay -0.1] [:west -0.1]} 
           (set (sut s))))))