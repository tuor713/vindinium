(ns vindinium.model)

(defn hero
  [game hero-id]
  (get-in game [:heroes hero-id]))
(defn heroes [game] (vals (:heroes game)))

(defmacro def-hero-accessor [key & [def-name]]
  (let [n (or def-name (symbol (name key)))]
    `(defn ~n
       ([hero#] (~key hero#))
       ([game# hero-id#] (~n (hero game# hero-id#))))))

(def-hero-accessor :id)
(def-hero-accessor :pos)
(def-hero-accessor :gold)
(def-hero-accessor :life)
(def-hero-accessor :spawnPos spawn-pos)
(def-hero-accessor :mineCount mine-count)


(defn tile 
  ([board x y] (get-in board [x y]))
  ([board [x y]] (get-in board [x y])))


(defn board [game] (:board game))
(defn turn [game] (:turn game))

(defn mine? [tile] (and (vector? tile) (= :mine (first tile))))

(defn my-id [state] (get-in state [:hero :id]))
(defn game [state] (:game state))
