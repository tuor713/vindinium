(ns vindinium.test-bots
  (:require [vindinium.model :as m]
            [vindinium.simulation :as sim])
  (:use clojure.test
        vindinium.bots))

(deftest test-board-env
  (let [b [[:empty :empty :empty :empty :empty]
           [:empty :wall :empty [:hero 1] :empty]
           [:empty :empty [:mine nil] :tavern :empty]
           [:empty :empty :empty :empty :empty]
           [:empty :empty :empty :empty :empty]]]
    (is (= #{[0 2] [1 3] [2 2] [1 2]}
           (board-env b 1 [1 2])))
    (is (= #{[0 2] [1 3] [2 2]}
           (board-env b 1 [1 2] true)))
    (is (= #{[0 1] [0 3] [2 3] [1 4]}
           (board-env b 2 [1 2] true)))
    (is (= #{[0 1] [0 3] [2 3] [1 4] [1 2] [0 2] [1 3] [2 2]}
           (board-env b 2 [1 2])))
    (is (= #{[0 0] [0 4] [2 4]}
           (board-env b 3 [1 2] true)))))

(deftest test-shortest-path
  (let [b [[:empty [:hero 1] :empty :wall :empty]
           [:empty [:mine nil] [:mine nil] :tavern :empty]
           [:empty :empty :empty :empty :empty]
           [:empty :wall :wall :wall :wall]
           [:empty :empty [:mine nil] :empty :empty]]]
    (is (nil? (shortest-path b [0 0] [4 4])))
    (is (= [[0 0] [1 0] [2 0] [2 1] [2 2] [2 3] [2 4] [1 4] [0 4]]
           (shortest-path b [0 0] [0 4])))
    (is (= [[0 0] [0 1] [0 2]]
           (shortest-path b [0 0] [0 2])))
    (is (nil? (shortest-path b #(and (not= % [0 1]) (passable? b %)) 
                             [0 0] [0 2])))
    (is (= [[0 0] [1 0] [2 0] [3 0] [4 0] [4 1] [4 2]]
           (shortest-path b [0 0] [4 2])))))

(deftest test-reachables
  (let [b [[:empty [:hero 1] :empty :wall :empty]
           [:empty [:mine nil] [:mine nil] :tavern :empty]
           [:empty :empty :empty :empty :empty]
           [:empty :wall :wall :wall :wall]
           [:empty :empty [:mine nil] :empty [:mine nil]]]]
    (is (= #{[1 1] [1 2] [4 2]}
           (reachables b #(m/mine? (m/tile b %)) [0 0])))
    (is (= #{[4 2] [4 4]}
           (reachables b #(m/mine? (m/tile b %)) [4 3])))
    (is (= #{[1 1] [1 2]}
           (reachables b #(m/mine? (m/tile b %)) 
                       #(and (passable? b %) (not= % [3 0]))
                       [0 0])))))



(deftest test-tavern-finder
  (is (= [[0 0] [0 1] [0 2] [1 2] [2 2]]
         (path-to-tavern 
          [[:empty :empty :empty]
           [:empty :wall :empty]
           [:empty :wall :tavern]]
          #{}
          [0 0])))
  (is (= [[0 0] [1 0] [2 0] [3 0] [3 1] [3 2] [3 3] [2 3] [1 3]]
         (path-to-tavern 
          [[:empty :empty :empty :hero]
           [:empty :empty :empty :tavern]
           [:empty :wall :wall :empty]
           [:empty :empty :empty :empty]]
          #{[0 2] [1 2]}
          [0 0])))
  (is (= [[:east 1]]
         ((tavern-finder 100) 
          {:game {:board
                  [[:hero :empty :empty :empty]
                   [:empty :wall :empty :tavern]
                   [:empty :wall :wall :empty]
                   [:empty :empty :empty :empty]]
                  :heroes {1 {:id 1 :pos [0 0] :life 20}}}
           :hero {:id 1}})))
  (is (= #{[:east 1] [:south 1]}
         (set ((tavern-finder 100) 
               {:game {:board
                       [[:hero :empty :empty :empty]
                        [:empty :empty :empty :tavern]
                        [:empty :wall :wall :empty]
                        [:empty :empty :empty :empty]]
                       :heroes {1 {:id 1 :pos [0 0] :life 20}}}
                :hero {:id 1}}))))
  (is (= [[:south 1]]
         ((tavern-finder 100) 
          {:game {:board
                  [[:hero :empty :empty :hero]
                   [:empty :wall :empty :tavern]
                   [:empty :wall :wall :empty]
                   [:empty :empty :empty :empty]]
                  :heroes {1 {:id 1 :pos [0 0] :life 20}
                           2 {:id 2 :pos [0 3] :life 100}}}
           :hero {:id 1}}))))

(deftest test-mine-finder
  (is (= [[0 0] [0 1] [0 2] [1 2] [2 2]]
         (path-to-mine 
          [[[:hero 1] :empty :empty]
           [:empty :wall :empty]
           [:empty :wall [:mine nil]]]
          1
          #{}
          [0 0])))
  
  (testing "gold predict"
    (is (= (+ 19 14) 
           (gold-predict 20 [[[0 0] [1 0] [2 0]] 
                             [[1 0] [0 0] [0 1] [0 2] [1 2] [2 2]]]))))

  (testing "harvest path search"
    (is (= [[[0 0] [1 0] [2 0] [3 0]]
            [[2 0] [1 0] [1 1] [1 2] [1 3] [2 3] [3 3]]
            [[2 3] [1 3] [0 3] [0 2] [0 1]]]
           (harvest-path-search 
            [[[:hero 1] :tavern :empty :empty]
             [:empty :empty :empty :empty]
             [:empty :wall :wall :empty]
             [[:mine nil] :wall :wall [:mine nil]]]
            1 100 #{} [0 0]))
        "Vanilla case get all results that can be produced")
    (is (= [[[0 0] [1 0] [2 0] [3 0]]
            [[2 0] [1 0] [1 1] [1 2] [2 2] [2 3] [3 3]]
            [[2 3] [2 4]]]
           (harvest-path-search
            [[[:hero 1] :tavern :empty :wall :empty]
             [:empty :empty :empty :wall :empty]
             [:empty :wall :empty :empty :tavern]
             [[:mine nil] :wall :empty [:mine nil] :empty]]
            1 100 #{} [0 0]))
        "Case where mining position matters for best access to next tavern")
    (is (= [[[0 0] [1 0] [2 0] [3 0]] 
            [[2 0] [1 0] [1 1] [1 2] [1 3] [2 3] [3 3]] 
            [[2 3]]]
           (harvest-path-search
            [[[:hero 1] :wall :empty :empty]
             [:empty :empty :empty :empty]
             [:empty :wall :wall :empty]
             [[:mine nil] :wall :wall [:mine nil]]]
            1 100 #{} [0 0]))
        "No tavern case"))

  (let [board [[[:hero 1] :empty :empty :tavern]
               [:empty :wall :empty :empty]
               [[:mine nil] :wall [:mine nil] :empty]]
        g {:heroes {1 {:id 1 :pos [0 0] :mineCount 0 :life 100 :gold 0}}
           :board board}
        g' (sim/step g 1 :south)]
    (is (= [[:south 1]]
           (mine-finder {:hero {:id 1} :game g})))
    (is (= [[:south 15]]
           (mine-finder {:hero {:id 1} :game g'})))
    (is (= [[:north 1]]
           (mine-finder {:hero {:id 1} :game (assoc-in g' [:board 2 0] [:mine 1])})))
    (is (= [[:south -100]]
           (mine-finder {:hero {:id 1} :game (assoc-in g' [:heroes 1 :life] 20)}))
        "Don't attack a mine on <= life")
    (is (= [[:south -100]]
           (mine-finder {:hero {:id 1} :game (-> g'
                                                 (assoc-in [:heroes 2] {:pos [1 1] :life 100})
                                                 (assoc-in [:board 1 1] [:hero 2]))}))
        "Don't attack a mine with enemies nearby")))


(deftest test-can-win
  (is (can-win? 1 20 true))
  (is (can-win? 21 20 false))
  (is (not (can-win? 20 20 false)))
  (is (not (can-win? 1 21 true)))
  (is (can-win? 35 21 true))
  (is (can-win? 35 21 false))
  (is (not (can-win? 35 22 false))))

(let [g {:heroes {1 {:pos [1 0] :spawnPos [0 0] :life 30 :gold 10 :mineCount 2 :id 1}
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
           (enemy-mod-map g 30 (m/hero g 3))))
    
    (is (= {[3 1] 0 [2 2] 0}
           (enemy-mod-map g 50 (m/hero g 3))))

    (let [g' (assoc-in g [:heroes 3 :life] 15)]
      (is (= {[3 1] -1 [2 2] -1 [3 2] 0.5}
             (enemy-mod-map g' 20 (m/hero g' 3)))))

    (is (= {[2 0] 0.5 [1 0] -1 [3 1] 0.5 [3 2] -1 [2 1] -1}
           (enemy-mod-map g 30 (m/hero g 4))))
    (let [g' (assoc-in g [:board 3 1] :tavern)]
      (is (= {[2 0] -1 [1 0] -1 [2 1] 0}
             (enemy-mod-map g' 30 (m/hero g' 4)))))
    (let [g' (-> (assoc-in g [:board 3 1] :tavern)
                 (assoc-in [:heroes 4 :life] 15))]
      (is (= {[2 0] 0.5 [1 0] -1 [2 1] -1}
             (enemy-mod-map g' 15 (m/hero g' 4)))))

    (is (= {[2 2] -2, [1 0] -1, [1 1] -1, [0 3] -1, [0 2] -1, [2 0] 0.5, [3 1] -0.5, [2 1] -1, [1 2] -1, [3 2] -1}
           (enemies-mod-map g 1)))
    
    (is (= [[:south 0.5] [:east -1] [:stay -1]]
           (combat-one-oh-one 
            {:game g
             :hero {:id 1}})))
    (is (= [[:south 0.5]]
           (combat-one-oh-one 
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

(deftest test-find-traps
  (is (= {:unsafe #{[0 0] [0 1] [0 3]} :semi-unsafe #{}}
         (find-traps [[:empty :empty :wall :empty]
                      [:empty :empty :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :empty :empty :empty]]))
      "Vanilla case")
  (is (= {:unsafe #{[0 3]} :semi-unsafe #{}}
         (find-traps [[:empty :tavern :wall :empty]
                      [:empty :empty :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :empty :empty :empty]]))
      "Tavern handling")

  (is (= {:unsafe #{[2 2]} :semi-unsafe #{}}
         (find-traps [[:empty :empty :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :wall :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :empty :empty :empty]]))
      "Canonical trap in inner wall")

  (is (= {:unsafe #{} :semi-unsafe #{[1 2] [2 2] [0 3] [1 3]}}
         (find-traps [[:empty :empty :empty :empty]
                      [:empty :wall :empty :empty]
                      [:empty :wall :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :empty :empty :empty]]))
      "Non-canonical trap in inner wall")

  (is (= {:unsafe #{[0 1] [1 0] [1 1] [1 2] [2 0] [3 0] [4 0] [5 0] [5 1]} :semi-unsafe #{}}
         (find-traps [[:wall :empty :wall :wall :wall]
                      [:empty :empty :empty :wall :wall]
                      [:empty :wall :wall :wall :wall]
                      [:empty :wall :empty :empty :empty]
                      [:empty :wall :empty :wall :empty]
                      [:empty :empty :empty :empty :empty]]))
      "Unsafe trough")

  (is (= {:unsafe #{} :semi-unsafe #{}}
         (find-traps [[:empty :empty :empty :wall]
                      [:empty :wall :empty :wall]
                      [:empty :wall :empty :empty]
                      [:empty :wall :wall :empty]
                      [:empty :empty :empty :empty]]))
      "Round about safetly")

  (is (= {:unsafe #{[0 1] [0 3] [0 4] [0 5] [0 6] [0 7] [0 8] [0 10]} :semi-unsafe #{}}
         (find-traps [[:wall :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :wall]
                      [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]
                      [:empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty]
                      [:empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty]]))
      "Round about safetly"))