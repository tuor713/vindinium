(ns vindinium.model)

(defn hero [game hero-id]
  (get-in game [:heroes hero-id]))

(defn hero= [game hero-id value]
  (assoc-in game [:heroes hero-id] value))

(defn heroes [game] (vals (:heroes game)))

(defmacro def-hero-accessor [key & [def-name]]
  (let [n (or def-name (symbol (name key)))
        n2 (symbol (str (name n) "="))]
    `(do (defn ~n
           ([hero#] (~key hero#))
           ([game# hero-id#] (~n (hero game# hero-id#))))
         (defn ~n2
           ([hero# value#] (assoc hero# ~key value#))
           ([game# hero-id# value#] 
              (hero= game# hero-id# (~n2 (hero game# hero-id#) value#)))))))

(def-hero-accessor :id)
(def-hero-accessor :pos)
(def-hero-accessor :gold)
(def-hero-accessor :life)
(def-hero-accessor :spawnPos spawn-pos)
(def-hero-accessor :mineCount mine-count)


(defn tile 
  ([board x y] (get-in board [x y]))
  ([board pos] (get-in board pos)))

(defn tile=
  ([board x y v] (assoc-in board [x y] v))
  ([board pos v] (assoc-in board pos v)))

(defn board [game] (:board game))
(defn board= [game board] (assoc game :board board))

(defn turn [game] (:turn game))

(defn mine? [tile] (and (vector? tile) (= :mine (first tile))))
(defn enemy-mine? [id tile] (and (mine? tile) (not= [:mine id] tile)))
(defn hero? [tile] (and (vector? tile) (= :hero (first tile))))
(defn enemy-hero? [id tile] (and (hero? tile) (not= [:hero id] tile)))

(defn my-id [state] (get-in state [:hero :id]))
(defn game [state] (:game state))
