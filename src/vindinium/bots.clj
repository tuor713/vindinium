(ns vindinium.bots
  (:require [vindinium.model :as m]))

(set! *warn-on-reflection* true)

;; agents

;; the simplest fallback
(defn rand-move [] (rand-nth ["north" "south" "east" "west" "stay"])) 

(defn heuristic-agent [& heuristics]
  (fn [input]
    (let [candidates
          (apply concat
                 (pmap 
                  (fn [f]
                    (try
                      (let [start (System/currentTimeMillis)
                            weight (get (meta f) :weight 1)
                            label (get (meta f) :label (str f))
                            choices (f input)
                            res 
                            (if (or (string? choices) (keyword? choices))
                              [[choices weight]]
                              (vec (map (fn [[move w]] [move (* w weight)]) choices)))
                            time (- (System/currentTimeMillis) start)]
                        (println (str "heuristic " label " (" time "ms) => " res))
                        res)
                      (catch Exception e
                        (println (str "Error in heuristic " (get (meta f) :label (str f)) ", ignoring it"))
                        (.printStackTrace e)
                        nil)))
                  heuristics))
          sum-weights
          (->> candidates
               (group-by first)
               (map (fn [[move ws]] [move (reduce + (map second ws))]))
               (shuffle))
          choice (first (apply max-key second sum-weights))]
      (println "heuristic sum =>" sum-weights "=>" choice)
      choice)))


;; movement

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


;; simulation

(defn update-board [board f]
  (vec
   (map
    (fn [row] (vec (map f row)))
    board)))

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

(defn move [game hero direction]
  (let [[x y] (m/pos game hero)
        [nx ny :as npos] (pos+ [x y] direction)
        terrain (m/tile (m/board game) npos)]
    (when-not (or (nil? terrain) (= :wall terrain))
      (-> (cond
           (or (nil? terrain) (= :wall terrain)) game

           (= :empty terrain)
           (-> game
               (assoc-in [:heroes hero :pos] npos)
               (assoc-in [:board x y] :empty)
               (assoc-in [:board nx ny] [:hero hero]))

           (= :tavern terrain)
           (-> game
               (update-in [:heroes hero] #(if (> (:gold %) 1) 
                                            (assoc % :gold (- (:gold %) 2)
                                                   :life (min 100 (+ (:life %) 50)))
                                            %)))

           (= :hero (first terrain))
           game 

           (and (= :mine (first terrain)) (= hero (second terrain)))
           game

           (and (= :mine (first terrain)) (> (:life (m/hero game hero)) 20))
           (-> game
               (assoc-in [:board nx ny] [:mine hero])
               (update-in [:heroes hero :life] #(- % 20))
               (update-in [:heroes hero :mineCount] inc))

           (and (= :mine (first terrain)) (<= (:life (m/hero game hero)) 20))
           (die game hero nil))

          (fight hero)

          ;; mines
          (update-in [:heroes hero] #(assoc % :gold (+ (:gold %) (:mineCount %))))

          ;; thirst
          (update-in [:heroes hero :life] #(max 1 (dec %)))))))

(defn remove-hero [game hero-id]
  (-> game
      (update-in [:heroes] #(dissoc % hero-id))
      (update-in [:board] #(update-board % (fn [terrain]
                                             (if (= terrain [:hero hero-id]) :empty terrain))))))


;; tree searching agents

(defn score [game hero-id]
  (let [hero (m/hero game hero-id)
        score (+ (* 10 (:gold hero))
                 (* 200 (:mineCount hero))
                 (:life hero))]
    score))

(defn max-score-dfs [game hero-id depth]
  (if (= depth 0)
    (score game hero-id)
    (->> move-options
         (keep #(move game hero-id %))
         (map #(max-score-dfs % hero-id (dec depth)))
         (apply max))))

(defn autist-dfs-agent [depth]
  (fn [state]
    (let [my-id (m/my-id state)
          game (:game state)
          game' (reduce remove-hero game (remove #{my-id} [1 2 3 4]))
          rated-options
          (keep 
           (fn [dir]
             (if-let [game'' (move game' my-id dir)]
               [dir (max-score-dfs game'' my-id (dec depth))]))
           move-options)]
      rated-options)))


;; single idea heuristics

(defn bfs 
  ([state neighbours success?]
     (loop [visited #{} queue [[state []]]]
       (when-not (empty? queue)
         (let [[s p] (first queue)
               queue' (subvec queue 1)]
           (cond
            (success? s) (conj p s)
            (visited s) (recur visited queue')
            :else
            (let [ns (remove visited (neighbours s))]
              (recur (conj visited s)
                     (into queue'
                           (map #(-> [% (conj p s)]) ns))))))))))

(defn path-to-tavern [board starting]
  (bfs starting
       (fn [pos]
         (neighbours board pos
                     #(or (contains? #{nil :wall :hero} %)
                           (m/mine? %))))
       (fn [pos] (= :tavern (m/tile board pos)))))

(defn tavern-finder [life-threshold]
  (fn [state]
    (let [id (m/my-id state)
          game (m/game state)
          adjacent-tavern 
          (->> real-moves
               (filter #(= :tavern
                           (m/tile (m/board game) 
                                   (pos+ (m/pos game id) %))))
               (first))]
      ;; if we are adjacent give a little push so we actually use it
      (if (and adjacent-tavern (<= (m/life game id) 50))
        [[adjacent-tavern 20]]
        (when (<= (m/life game id) life-threshold)
          (move->direction (m/pos game id) 
                           (second (path-to-tavern (m/board game) (m/pos game id)))))))))

(defn path-to-mine [board hero-id starting]
  (bfs starting
       (fn [pos]
         (->> real-moves
              (map #(pos+ pos %))
              (remove #(contains? #{nil :wall :hero :tavern [:mine hero-id]} (m/tile board %)))))
       (fn [pos] (m/mine? (m/tile board pos)))))

(defn mine-finder [life-threshold]
  (fn [state]
    (let [id (m/my-id state)
          game (:game state)]
      (when-not (< (m/life game id) life-threshold)
        (move->direction (m/pos game id) 
                         (second (path-to-mine (m/board game) id (m/pos game id))))))))

(defn enemy-mod-map [game my-life enemy]
  (let [enemy-pos (m/pos enemy)
        ns (neighbours (m/board game) enemy-pos)
        ns2 (distinct (remove #{enemy-pos} (mapcat #(neighbours (m/board game) %) ns)))]
    (->> (concat
          ;; don't get into the +1 space on your own
          (map #(-> [% -1]) ns2)
          (cond
           (and (next-to-tavern? (m/board game) enemy-pos) 
                (> (m/life enemy) 20))
           (map #(-> [% -1]) ns)

           ;; we would loose even if we hit first
           (<= (quot my-life 20) (dec (quot (m/life enemy) 20)))
           (map #(-> [% -1]) ns)

           ;; we would win and there is something to get, but do go for the kill just to get rid 
           ;; of annoying bots
           (or (> (m/mine-count enemy) 0) 
               (<= (m/life enemy) 20))
           (map #(-> [% 0.5]) ns)

           ;; nothing to get, no preference ... to validate
           :else nil))
         (map (fn [[pos w]]
                (if (and (> my-life 20) (next-to-tavern? (m/board game) pos))
                  [pos (max w 0)]
                  [pos w])))
         (into {}))))

(defn enemies-mod-map [game hero-id]
  (let [my-life (m/life game hero-id)]
    (->> (m/heroes game)
         (remove #(= hero-id (m/id %)))
         (map #(enemy-mod-map game my-life %))
         (apply merge-with +))))

(defn combat-one-oh-one 
  "Basic close range combat:
- avoid entering a E+2 square
- prefer to enter a E+1 square if we can wind the combat (i.e. sufficiently more health) and the hero actually
  holds some mines (otherwise why bother)
- avoid entering an E+1 square when the enemy is next to a tavern
- eliminate negatives when mine-count = 0 i.e. nothing to loose"
  [input]
  (let [game (m/game input) 
        id (m/my-id input)
        mod-map (enemies-mod-map game id)]
    (->> move-options
         (keep #(if-let [game' (move game id %)]
                  [% game']))
         (keep (fn [[move g]]
                 (if-let [mod (mod-map (m/pos g id))]
                   [move mod])))
         (remove (fn [[move mod]]
                   (and (< mod 0) (= (m/mine-count game id) 0)))))))

(defn avoid-spawning-spots [ratio]
  (fn [input]
    (let [game (m/game input)
          my-id (m/my-id input)
          enemies (remove #(= my-id (:id %)) (m/heroes game))
          spawns (map m/spawn-pos enemies)]
      (->> move-options
           (keep #(if-let [g' (move game my-id %)]
                   (-> [% (m/pos g' my-id)])))
           (keep 
            (fn [[move pos]]
              (cond
               (some #{pos} spawns) [move -1]
               (some #(adjacent? (m/board game) pos %) spawns) [move (- ratio)]
               :else nil)))))))


;; alpha-beta searching

