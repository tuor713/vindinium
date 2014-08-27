(ns vindinium.bots
  (:require [vindinium.model :as m]
            [vindinium.simulation :as sim])
  (:use clojure.data.priority-map))

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
               ;; seed the list so we always have options present at default value
               (concat [[:north 0] [:south 0] [:west 0] [:east 0] [:stay 0]]) 
               (group-by first)
               (map (fn [[move ws]] [move (reduce + (map second ws))]))
               (shuffle))
          choice 
          (first (apply max-key second sum-weights))]
      (println "heuristic sum =>" sum-weights "=>" choice)
      choice)))

;; tree searching agents

(defn score [game hero-id]
  (let [hero (m/hero game hero-id)
        score (+ (* 10 (:gold hero))
                 (* 200 (:mineCount hero)))]
    score))

(defn max-score-dfs [game hero-id depth]
  (if (= depth 0)
    (score game hero-id)
    (->> sim/move-options
         (keep #(sim/step game hero-id %))
         (map #(max-score-dfs % hero-id (dec depth)))
         (apply max))))

(defn autist-dfs-agent [depth]
  (fn [state]
    (let [my-id (m/my-id state)
          game (:game state)
          game' (reduce sim/remove-hero game (remove #{my-id} [1 2 3 4]))
          rated-options
          (keep 
           (fn [dir]
             (if-let [game'' (sim/step game' my-id dir)]
               [dir (max-score-dfs game'' my-id (dec depth))]))
           sim/move-options)]
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

(defn manhattan-distance 
  [[^long x ^long y] [^long xx ^long yy]]
  (+ (Math/abs (- xx x))
     (Math/abs (- yy y))))

(defn passable? [board pos]
  (let [t (m/tile board pos)]
    (not (or (= nil t)
             (= :wall t)
             (= :tavern t)
             (m/mine? t)))))

(defn shortest-path
  ([board start end]
     (shortest-path board (partial passable? board) start end))
  ([board accept? start end]
     (loop [visited #{} queue (priority-map {:pos start :path [start]} (manhattan-distance start end))]
       (when-not (empty? queue)
         (let [[{pos :pos path :path :as state} _] (first queue)
               queue' (dissoc queue state)]
           (cond
            (= end pos) 
            path
            
            (contains? visited pos)
            (recur visited queue')
            
            :else
            (let [ns (->> sim/real-moves
                          (map #(sim/pos+ pos %))
                          (filter #(or (= end %) (accept? %)))
                          (remove visited))]
              (recur
               (conj visited pos)
               (into queue'
                     (map
                      #(-> [{:pos % :path (conj path %)}
                            (+ (count path) (manhattan-distance % end))])
                      ns))))))))))

(defn board-env 
  "Calculates the environment of reachable squares 
- including mines and taverns, but excluding squares beyond them
- including heros and squares beyond them"
  ([board num-steps pos]
     (board-env board num-steps pos false))
  ([board num-steps pos last-generation-only?]
     ((if last-generation-only? :last-cover :cover)
      (reduce
       (fn [{pos :positions cover :cover} _]
         (let [npos (mapcat #(sim/neighbours board % (fn [t] (or (nil? t) (= :wall t)))) pos)]
           {:cover (into cover npos)
            :last-cover (set (remove cover npos))
            :positions
            (->> npos
                 (remove #(let [t (m/tile board %)] (or (m/mine? t) (= :tavern t))))
                 (remove cover)
                 (distinct))}))
       {:positions [pos]
        :cover #{pos}}
       (range num-steps)))))

(defn can-win? 
  ;; internal call
  ([lifea lifeb]
     (if (<= lifea 0) 
       false
       (not (can-win? (- lifeb 20) (max (dec lifea) 1)))))
  ;; external call
  ([my-life enemy-life first-strike?]
     (if first-strike?
       (not (can-win? (- enemy-life 20) (max (dec my-life) 1)))
       (can-win? (- my-life 20) (max (dec enemy-life) 1)))))

(defn enemy-mod-map [game my-life enemy]
  (let [enemy-pos (m/pos enemy)
        enemy-life (m/life enemy)
        ns (sim/neighbours (m/board game) enemy-pos)
        ns2 (distinct (remove #{enemy-pos} (mapcat #(sim/neighbours (m/board game) %) ns)))
        win-first? (can-win? my-life enemy-life true)
        win-second? (can-win? (dec my-life) enemy-life false)]
    (->> (concat

          ;; only get into T+1 space if we can win even without first strike
          ;; TODO this needs to be made tavern aware i.e. if the ns space is next to a tavern
          ;; then don't get in there whatever
          (map #(-> (if win-second? [% 0] [% -1])) ns2)
          
          (cond
           (and (sim/next-to-tavern? (m/board game) enemy-pos) 
                (> enemy-life 20))
           (map #(-> [% -1]) ns)

           ;; we would loose even if we hit first
           (not win-first?)
           (map #(-> [% -1]) ns)

           ;; we would win and there is something to get, but do go for the kill just to get rid 
           ;; of annoying bots
           (or (> (m/mine-count enemy) 0) 
               (<= enemy-life 20))
           (map #(-> [% 0.5]) ns)

           ;; nothing to get, no preference ... to validate
           :else nil))

         ;; if next to a tavern in an encouter situation prefer that
         (map (fn [[pos w]]
                (if (and (> my-life 21) (sim/next-to-tavern? (m/board game) pos))
                  [pos (max w 0)]
                  [pos w])))
         
         (into {}))))

(defn enemies-mod-map [game hero-id]
  (let [my-life (m/life game hero-id)]
    (->> (m/heroes game)
         (remove #(= hero-id (m/id %)))
         (map #(enemy-mod-map game my-life %))
         (apply merge-with +))))

(defn tavern-neighbours [board blacklist pos]
  (->> sim/real-moves 
       (map #(sim/pos+ pos %))
       (remove #(let [tile (m/tile board %)]
                  (or (contains? blacklist %) 
                      (contains? #{nil :wall :hero} tile)
                      (m/mine? tile))))))

(defn path-to-tavern [board blacklist starting]
  (bfs starting
       (partial tavern-neighbours board blacklist)
       (fn [pos] (= :tavern (m/tile board pos)))))

(defn tavern-finder [life-threshold]
  (fn [state]
    (let [id (m/my-id state)
          game (m/game state)
          adjacent-tavern 
          (->> sim/real-moves
               (filter #(= :tavern
                           (m/tile (m/board game) 
                                   (sim/pos+ (m/pos game id) %))))
               (first))]
      ;; if we are adjacent give a little push so we actually use it
      (if (and adjacent-tavern (<= (m/life game id) 75))
        [[adjacent-tavern 20]]
        (when (<= (m/life game id) life-threshold)
          (let [blacklist (set (map key (filter #(< (val %) 0) (enemies-mod-map game id))))
                adj-pos (tavern-neighbours (m/board game) blacklist (m/pos game id))
                paths (keep #(path-to-tavern (m/board game) blacklist %) adj-pos)
                min-len (when (seq paths) (apply min (map count paths)))
                min-paths (filter #(= min-len (count %)) paths)]
            (map 
             #(-> [(sim/move->direction (m/pos game id) (first %)) 1])
             min-paths)))))))

(defn mine-neighbours [board hero-id blacklist pos]
  (->> sim/real-moves
       (map #(sim/pos+ pos %))
       (remove #(let [tile (m/tile board %)]
                  (or (contains? blacklist %)
                      (contains? #{nil :wall :tavern [:mine hero-id]} tile)
                      (and (m/hero? tile) (not= tile [:hero hero-id])))))))

(defn path-to-mine [board hero-id blacklist starting]
  (bfs starting
       (partial mine-neighbours board hero-id blacklist)
       (fn [pos] (m/mine? (m/tile board pos)))))

;; TODO deal with cases where the tavern is blocked
(defn full-path-search
  [max-result board hero-id life blacklist starting]
  (let [n (partial mine-neighbours board hero-id blacklist)
        n2 (partial tavern-neighbours board blacklist)]
    ;; TODO avoid duplicate tavern paths since those are least important only the quickest one counts
    (loop [visited-map {{:segments [] :returning? false} #{}}
           closed-segments #{}
           queue [{:pos starting :life life :path [] :segments [] :returning? false}] 
           res []]
      (if (empty? queue)
        (if (seq res)
          {:complete true :results res}
          (let [max-segs (apply max (map #(count (:segments %)) (keys visited-map)))]
            (if (> max-segs 0)
              {:complete false :results (->> (keys visited-map)
                                             (map :segments)
                                             (filter #(= max-segs (count %))))}
              {:complete false :results nil})))
        (let [{pos :pos 
               life :life 
               path :path 
               segments :segments 
               returning? :returning? 
               :as state} (first queue)
              queue' (subvec queue 1)
              visited-key {:segments segments :returning? returning?}
              visited (get visited-map visited-key #{})]
          (if returning?
            (cond
             (contains? closed-segments segments)
             (recur visited-map closed-segments queue' res)

             (= :tavern (m/tile board pos))
             (let [res' (conj res (conj segments (conj path pos)))]
               (if (>= (count res') max-result)
                 {:complete true :results res'}
                 (recur (assoc visited-map visited-key (conj visited pos))
                        (conj closed-segments segments)
                        queue' 
                        res')))

             (contains? visited pos)
             (recur visited-map closed-segments queue' res)

             :else
             (let [ns (remove visited (n2 pos))]
               (recur 
                (assoc visited-map visited-key (conj visited pos))
                closed-segments
                (into queue'
                      (map
                       #(-> {:pos %
                             :path (conj path pos)
                             :segments segments
                             :visited (conj visited pos)
                             :returning? true})
                       ns))
                res)))

            (cond
             (<= life 20) 
             (recur visited-map closed-segments queue' res)

             (contains? visited pos) 
             (recur visited-map closed-segments queue' res)

             (m/mine? (m/tile board pos))
             (let [path' (conj path pos)
                   segments' (conj segments path')

                   next-closest (path-to-mine board hero-id (into blacklist (map last segments')) (last path))
                   can-find-mine? 
                   (and (seq next-closest)
                        (<= (dec (count next-closest)) (- life 41)))

                   state' {:pos (last path)
                           :life (- life 21) 
                           :path []
                           :segments segments'
                           :returning? false}]
               (if can-find-mine?
                 (recur 
                  ;; don't update the mine position in the old map 
                  ;; since we may approach the same mine gainfully from more than one position
                  ;; bootstrap new visited map with all the mines already used, so there is no double counting
                  (assoc visited-map {:segments segments' :returning? false} 
                         (set (map last segments')))
                  closed-segments
                  (conj queue' state') 
                  res)
                 (recur
                  (assoc visited-map {:segments segments' :returning? true} #{})
                  closed-segments
                  (conj queue' (assoc state' :returning? true))
                  res)))

             :else
             (let [ns (remove visited (n pos))]
               (recur 
                (assoc visited-map visited-key (conj visited pos))
                closed-segments
                (into queue' 
                      (map #(-> {:pos % 
                                 :life (dec life) 
                                 :path (conj path pos) 
                                 :segments segments 
                                 :returning? false}) ns))
                      res)))))))))

(defn gold-predict [turns mining-path]
  (:gold
   (reduce
    (fn [{turns :turns gold :gold} path]
      (let [end-turn (- turns (dec (count path)))]
        {:turns end-turn
         :gold (+ gold (inc end-turn))}))
    {:turns 20 :gold 0}
    mining-path)))

(defn mine-path-fitness [segments]
  (+ (gold-predict 100 (butlast segments))
     ;; todo the divisor is somewhat arbitrary
     (- (/ (count (last segments)) 3.0))))

(defn mine-finder 
  ([state] (mine-finder 10 state))
  ([top-n-cutoff state]
     (let [id (m/my-id state)
           game (:game state)
           board (m/board game)

           adjacent-mines
           (filter #(let [t (m/tile board %)] (and (m/mine? t) (not= t [:mine id]))) 
                   (board-env board 1 (m/pos game id)))
           enemies-near (filter #(let [t (m/tile board %)]
                                   (and (m/hero? t) (not= t [:hero id])))
                                (board-env board 2 (m/pos game id)))]
       (cond
        (and (seq adjacent-mines) 
             (> (m/life game id) 20) 
             (not (seq enemies-near)))
        [[(sim/move->direction (m/pos game id) (first adjacent-mines)) 2]]

        ;; life <= 20 or enemies are near
        ;; either way don't do a mine just yet
        (seq adjacent-mines) 
        (map #(-> [(sim/move->direction (m/pos game id) %) -100]) adjacent-mines)

        :else 
        (let [blacklist (set (map key (filter #(< (val %) 0) (enemies-mod-map game id))))
              adj-pos (mine-neighbours (m/board game) id blacklist (m/pos game id))

              paths 
              (let [{complete? :complete paths :results} 
                    (full-path-search top-n-cutoff (m/board game) id (m/life game id) blacklist (m/pos game id))]
                (if complete?
                  paths
                  (map (fn [p] (conj p [])) paths)))
              
              max-fitness (when (seq paths) (apply max (map #(mine-path-fitness %) paths)))
              max-paths (filter #(= max-fitness (mine-path-fitness %)) paths)]
          (println "mining debug:" max-fitness max-paths paths)
          (->> max-paths
               (map first)
               (mapcat 
                (fn [path]
                  (let [ps (map #(shortest-path board 
                                                (fn [pos] (and (passable? board pos)
                                                               (not (contains? blacklist pos))
                                                               (let [t (m/tile board pos)]
                                                                 (or (not (m/hero? pos)) (= [:hero id] t)))))
                                                ;; search for the penultimate spot so we end up with the same 
                                                % (last (butlast path)))
                                adj-pos)
                        min-len (apply min (map count ps))]
                    (map first (filter #(= min-len (count %)) ps)))))
               (distinct)
               (map #(-> [(sim/move->direction (m/pos game id) %) 1]))))))))



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
    (->> sim/move-options
         (keep #(if-let [game' (sim/step game id %)]
                  ;; check that we are not taking dying as an option to kill someone else, yet
                  (when (>= (m/mine-count game' id) (m/mine-count game id))
                    [% game'])))
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
      (->> sim/move-options
           (keep #(if-let [g' (sim/step game my-id %)]
                   (-> [% (m/pos g' my-id)])))
           (keep 
            (fn [[move pos]]
              (cond
               (some #{pos} spawns) [move -1]
               (some #(sim/adjacent? (m/board game) pos %) spawns) [move (- ratio)]
               :else nil)))))))


;; alpha-beta searching

