(ns vindinium.simulation
  "Namespace for the simulation of the world"
  (:require [vindinium.model :as m]))

(def moves 
  {:north [-1 0]
   :south [1 0]
   :east [0 1]
   :west [0 -1]
   :stay [0 0]})

(def move-options (vec (keys moves)))
(def real-moves [:north :south :east :west])

(defn pos+ [[x y] dir]
  (let [[dx dy] (moves dir)]
    [(+ x dx) (+ y dy)]))

(defn move->direction [start end]
  (some #(when (= end (pos+ start %)) %) real-moves))

(defn valid-move? [board [x y] move]
  (let [npos (pos+ [x y] move)
        square (get-in board npos)]
    (cond
     (or (nil? square) (= :wall square)) false
     (= :empty square) npos
     :else [x y])))

(defn valid-moves [board pos]
  (filter (partial valid-move? board pos) [:north :south :east :west]))

(defn adjacent? [board pos otherpos]
  (some #{otherpos} (map #(pos+ pos %) real-moves)))

(defn next-to-tavern? [board pos]
  (some #(= :tavern (m/tile board (pos+ pos %))) real-moves))


(defn neighbours 
  ([board pos] 
     (neighbours board 
                 pos 
                 (fn [tile]
                   (or (nil? tile)
                       (= :wall tile)
                       (= :tavern tile)
                       (m/mine? tile)))))
  ([board pos exclude?]
     (->> real-moves
          (map #(pos+ pos %))
          (remove #(exclude? (m/tile board %))))))

(defn update-board [board f]
  (vec
   (map
    (fn [row] (vec (map f row)))
    board)))


;; Simulation proper

(defn die [game hero vanquisher]
  (let [vf (if (nil? vanquisher)
             identity
             #(update-in % [:heroes vanquisher :mineCount] (partial + (m/mine-count game hero))))
        h (m/hero game hero)
        on-spawn-pos (m/tile (m/board game) (m/spawn-pos game hero))
        spawn-kill
        (if (and (m/hero? on-spawn-pos) (not= (second on-spawn-pos) hero))
          #(die % (second on-spawn-pos) hero)
          identity)
        ]
    (-> game
        (update-in [:heroes hero]
                   #(assoc % :life 100
                           :pos (:spawnPos %)
                           :mineCount 0))
        (update-in [:board]
                   (fn [board]
                     (-> board
                         ;; careful to update only if we are still hero, we could have been
                         ;; removed by a spawn kill
                         (update-in (:pos h) #(if (= % [:hero hero]) :empty %))
                         (assoc-in (:spawnPos h) [:hero hero]))))
        (vf)
        (update-in [:board] (fn [board] 
                              (update-board board #(get {[:mine hero] [:mine vanquisher]} % %))))
        (spawn-kill))))

(defn fight [game hero]
  (let [[x y] (m/pos game hero)]
    (reduce
     (fn [g dir]
       (let [[dx dy] (moves dir)
             [nx ny :as npos] [(+ x dx) (+ y dy)]
             terrain (get-in (m/board game) npos)]
         (if (and (vector? terrain) (= :hero (first terrain)))
           (let [foe-id (second terrain)
                 foe (m/hero game foe-id)]
             (if (<= (:life foe) 20)
               (die g foe-id hero)
               (-> g
                   (update-in [:heroes foe-id :life] #(- % 20)))))
           g)))
     game
     [:north :south :west :east])))

(defn end-of-turn [game hero-id]
  (update-in game
             [:heroes hero-id] 
             (fn [hero]
               (-> hero
                   ;; mines 
                   (m/gold= (+ (m/gold hero) (m/mine-count hero)))
                   ;; thirst
                   (m/life= (max 1 (dec (m/life hero))))))))

(defn drink [hero]
  (if (> (m/gold hero) 1)
    (-> hero
        (m/gold= (- (m/gold hero) 2))
        (m/life= (min 100 (+ (m/life hero) 50))))
    hero))

(defn move [game hero-id pos npos]
  (-> game
      (m/pos= hero-id npos)
      (m/board= 
       (-> (m/board game)
           (m/tile= pos :empty)
           (m/tile= npos [:hero hero-id])))))

(defn conquer [game hero-id mine-pos]
  (let [hero (m/hero game hero-id)]
    (-> game
        (m/board= (m/tile= (m/board game) mine-pos [:mine hero-id]))
        (m/hero=
         hero-id
         (-> hero
             (m/life= (- (m/life hero) 20))
             (m/mine-count= (inc (m/mine-count hero))))))))

(defn step 
  ([game hero-id direction]
     (step game hero-id direction false))
  ([game hero-id direction include-invalid?]
     (let [[x y :as pos] (m/pos game hero-id)
           [nx ny :as npos] (pos+ [x y] direction)
           terrain (m/tile (m/board game) npos)
           hero (m/hero game hero-id)

           ;; moves are invalid that lead to the same result as :stay (exclude that case deliberately)
           ;; - moving off the map
           ;; - moving into a wall
           ;; - moving into a mine already owned
           invalid? 
           (and
            (not= npos [x y])
            (or (nil? terrain) 
                (= :wall terrain) 
                (m/hero? terrain)
                (and (m/mine? terrain) (= hero-id (second terrain)))))]
       (when-not (and (not include-invalid?) invalid?)
         (-> (cond
              ;; no-op
              (or (= npos [x y]) invalid?) game

              (= :empty terrain)
              (move game hero-id pos npos)

              (= :tavern terrain)
              (m/hero= game hero-id (drink hero))

              (and (m/mine? terrain) (> (m/life hero) 20))
              (conquer game hero-id npos)

              (and (m/mine? terrain) (<= (m/life hero) 20))
              (die game hero-id nil))

             (fight hero-id)
             (end-of-turn hero-id))))))


(defn remove-hero [game hero-id]
  (-> game
      (update-in [:heroes] #(dissoc % hero-id))
      (update-in [:board] #(update-board % (fn [terrain]
                                             (if (= terrain [:hero hero-id]) :empty terrain))))))