(ns vindinium.samples)

(def large-map-game
  {:id "cc799n65", :turn 3, :maxTurns 1200, 
   :heroes {4 {:name "malvert.1.0.4", :pos [3 22], :crashed false, :life 100, :gold 0, :id 4, 
               :spawnPos [3 22], :elo 997, :userId "tvz140nm", :mineCount 0}, 
            3 {:name "malvert.1.0.2", :pos [25 22], :crashed false, :life 99, :gold 0, :id 3, 
               :spawnPos [24 22], :elo 1215, :userId "439p4zz9", :mineCount 0}, 
            2 {:name "malvert.1.0.1", :pos [25 5], :crashed false, :life 99, :gold 0, :id 2, 
               :spawnPos [24 5], :elo 1245, :userId "dluuynl2", :mineCount 0}, 
            1 {:name "malvert.1.0.0", :pos [2 5], :crashed false, :life 99, :gold 0, :id 1, 
               :spawnPos [3 5], :elo 1347, :userId "l13o6ibc", :mineCount 0}}, 
   :board [[:empty [:mine nil] :empty :empty :empty :empty [:mine nil] :empty :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :empty [:mine nil] :empty :empty :empty :empty [:mine nil] :empty]
           [:empty :empty :empty :empty :empty :empty :wall :empty :empty :wall :empty :empty [:mine nil] :empty :empty [:mine nil] :empty :empty :wall :empty :empty :wall :empty :empty :empty :empty :empty :empty]
           [:empty :wall :empty :empty :empty [:hero 1] :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty]
           [:empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty [:hero 4] :empty :empty :empty :empty :empty]
           [:wall :empty :empty :empty :wall :wall :empty :empty :empty :tavern :empty :empty :empty :empty :empty :empty :empty :empty :tavern :empty :empty :empty :wall :wall :empty :empty :empty :wall]
           [[:mine nil] :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty [:mine nil]]
           [:empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty]
           [:empty :wall :empty :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :wall :empty :empty :wall :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty :empty :wall :empty]
           [:empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty]
           [:empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty]
           [:empty :empty :empty [:mine nil] :empty :empty :empty :empty :wall :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :wall :empty :empty :empty :empty [:mine nil] :empty :empty :empty]
           [:wall :empty :empty :wall :empty :empty :empty :wall :wall :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :wall :wall :empty :empty :empty :wall :empty :empty :wall]
           [[:mine nil] :empty :empty :empty :empty :wall :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :wall :empty :empty :empty :empty [:mine nil]]
           [:wall :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :wall]
           [:wall :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :wall]
           [[:mine nil] :empty :empty :empty :empty :wall :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :wall :empty :empty :empty :empty [:mine nil]]
           [:wall :empty :empty :wall :empty :empty :empty :wall :wall :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :wall :wall :empty :empty :empty :wall :empty :empty :wall]
           [:empty :empty :empty [:mine nil] :empty :empty :empty :empty :wall :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :wall :empty :empty :empty :empty [:mine nil] :empty :empty :empty]
           [:empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty]
           [:empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty]
           [:empty :wall :empty :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :wall :empty :empty :wall :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty :empty :wall :empty]
           [:empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :wall :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty]
           [[:mine nil] :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty [:mine nil]]
           [:wall :empty :empty :empty :wall :wall :empty :empty :empty :tavern :empty :empty :empty :empty :empty :empty :empty :empty :tavern :empty :empty :empty :wall :wall :empty :empty :empty :wall]
           [:empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :empty]
           [:empty :wall :empty :empty :empty [:hero 2] :empty :empty :wall :empty :empty :empty :empty :empty :empty :empty :empty :empty :empty :wall :empty :empty [:hero 3] :empty :empty :empty :wall :empty]
           [:empty :empty :empty :empty :empty :empty :wall :empty :empty :wall :empty :empty [:mine nil] :empty :empty [:mine nil] :empty :empty :wall :empty :empty :wall :empty :empty :empty :empty :empty :empty]
           [:empty [:mine nil] :empty :empty :empty :empty [:mine nil] :empty :empty :empty [:mine nil] :empty :empty :empty :empty :empty :empty [:mine nil] :empty :empty :empty [:mine nil] :empty :empty :empty :empty [:mine nil] :empty]], 
   :finished false})