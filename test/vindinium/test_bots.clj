(ns vindinium.test-bots
  (:require [vindinium.model :as m])
  (:use clojure.test
        vindinium.bots))

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
          [[:empty :empty :empty]
           [:empty :wall :empty]
           [:empty :wall [:mine nil]]]
          1
          #{}
          [0 0]))))


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