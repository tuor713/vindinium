(ns vindinium.bots
  (:require [vindinium.model :as m]
            [vindinium.simulation :as sim]
            [clojure.set :as set])
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

(defn passable? 
  ([board pos]
     (passable? (m/tile board pos)))
  ([tile]
     (not (or (= nil tile)
              (= :wall tile)
              (= :tavern tile)
              (m/mine? tile)))))

(defn a*-search 
  ([neighbourhood distance cost start]
     (a*-search #(= 0 (distance %)) neighbourhood distance cost start))
  ([success? neighbourhood distance cost start]
     (loop [visited #{} queue (priority-map [start [start]] (distance start)) counter 0]
       (if (seq queue)
         (let [[[state path] _] (first queue)
               queue' (dissoc queue [state path])]
           (cond
            (success? state) 
            (do
              #_(println "A* search:" counter "nodes, result:" path)
              path)
            
            (contains? visited state)
            (recur visited queue' (inc counter))

            :else
            (let [ns (remove visited (neighbourhood state))
                  queue'' (into queue' 
                                (map 
                                 #(-> [[% (conj path %)] (+ (cost (conj path %)) (distance %))])
                                 ns))]
              (recur (conj visited state) queue'' (inc counter)))))
         (do #_(println "A* search:" counter "nodes, no result")
             nil)))))

(defn shortest-path
  ([board start end]
     (shortest-path board (partial passable? board) start end))
  ([board accept? start end]
     (a*-search
      (fn [state] (= end state))
      (fn [state]
        (->> sim/real-moves
             (map #(sim/pos+ state %))
             (filter #(or (= end %) (accept? %)))))
      (fn [state] (manhattan-distance state end))
      (fn [path] (dec (count path)))
      start)))

(defn reachables 
  ([board success? start]
     (reachables board success? (partial passable? board) start))
  ([board success? passable? start]
     (loop [visited #{} queue [start] res #{}]
       (if (seq queue)
         (let [pos (first queue)
               queue' (subvec queue 1)
               visited' (conj visited pos)]
           (cond
            (contains? visited pos)
            (recur visited queue' res)

            (success? pos)
            (recur visited' queue' (conj res pos))

            :else
            (let [ns (->> sim/real-moves
                          (map #(sim/pos+ pos %))
                          (filter #(or (success? %) (passable? %)))
                          (remove visited))]
              (recur visited' (into queue' ns) res))))
         res))))


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
        [[adjacent-tavern 10]]
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
                      (m/enemy-hero? hero-id tile))))))

(defn path-to-mine [board hero-id blacklist starting]
  (bfs starting
       (partial mine-neighbours board hero-id blacklist)
       (fn [pos] (m/mine? (m/tile board pos)))))


(defn harvest-path-search
  "Mining path search based on A* search. This is not very efficient A* search by any means.
- Distance to goal is estimated (crudely) by the manhattan distance to nearest tavern to close out (or 1 if there are no taverns)
- Distance travelled takes into account gold produced to favour paths of high produce (5 - no of active mines)."
  [board hero-id life blacklist starting]
  (let [taverns (reachables board 
                            #(= :tavern (m/tile board %))
                            #(and (not (blacklist %)) (passable? board %))
                            starting)
        _ (println "Reachable taverns: " taverns)
        reachable-mines (reachables board 
                                    #(m/enemy-mine? hero-id (m/tile board %))
                                    #(and (not (blacklist %)) (passable? board %))
                                    starting)
        no-reachable-mines (count reachable-mines)
        return-state (atom {:started #{} :forbidden #{}})
        result (a*-search
                ;; success criterion
                (fn [state] 
                  (and (:returning? state) 
                       (or (= :tavern (m/tile board (:pos state)))
                           (empty? taverns))))
                
                ;; neighbourhood
                (fn [state]
                  (let [{cost :cost paths :paths life :life reset-state :reset-state} (meta state)
                        {pos :pos mines :mines returning? :returning?} state]
                    (cond 
                     (:returning? state)
                     (map
                      #(with-meta 
                         {:pos % :mines mines :returning? returning?}
                         {:cost (+ cost (- 5 (count mines))) 
                          :life (max (dec life) 1) 
                          :reset-state reset-state
                          :paths (conj (pop paths) (conj (last paths) %))})
                      (tavern-neighbours board blacklist pos))

                     (<= life 20) ;; mining, but out of life, check whether we have started search for a return already
                     (when-not (or (contains? (:forbidden @return-state) mines)
                                   (contains? (:started @return-state) mines)
                                   (empty? mines))
                       (swap! return-state update-in [:started] conj mines)
                       ;; reset us to the place just after the last mine
                       [(with-meta {:pos (first (last paths)) :mines mines :returning? true}
                          (assoc reset-state :reset-state reset-state))])

                     :else ;; mining
                     (map
                      #(if (m/mine? (m/tile board %))
                         (let [proto-meta {:cost (+ cost (- 5 (count mines) 1)) 
                                           :life (max (- life 21) 1)
                                           :paths (into (pop paths) [(conj (last paths) %) [pos]])}
                               meta' (assoc proto-meta :reset-state proto-meta)]
                           (if (>= (inc (count mines)) no-reachable-mines)
                             (do 
                               (swap! return-state update-in [:started] conj mines)
                               ;; don't change position, mines contain the mine itself and the access position
                               (with-meta {:pos pos :mines (conj mines [% pos]) :returning? true} meta'))
                             
                             (do
                               (swap! return-state update-in [:forbidden] conj mines)
                               ;; don't change position, mines contain the mine itself and the access position
                               (with-meta {:pos pos :mines (conj mines [% pos]) :returning? false} meta'))))
                         (with-meta
                           {:pos % :mines mines :returning? false}
                           {:cost (+ cost (- 5 (count mines))) 
                            :life (max (dec life) 1) 
                            :reset-state reset-state
                            :paths (conj (pop paths) (conj (last paths) %))}))
                      (mine-neighbours board hero-id (into blacklist (map first mines)) pos)))))
                
                ;; distance estimate: steps to close out at a tavern
                (fn [state] 
                  (let [mine-count (count (:mines state))
                        mult (- 5 mine-count)]
                    (if (not (empty? taverns))
                      (->> taverns
                           (map #(* 1 (manhattan-distance (:pos state) %)))
                           (cons 1)
                           (apply min))
                      1)))

                ;; cost
                (fn [path] (:cost (meta (last path))))

                ;; state representation
                ;; we use meta information to keep some information from polluting the main state
                (with-meta
                  {:pos starting
                   :mines #{}
                   :returning? false}
                  {:cost 0 :paths [[starting]] :life life
                   :reset-state {:cost 0 :paths [[starting]] :life life}}))]
    (when result
      (:paths (meta (last result))))))


(defn gold-predict [turns mining-path]
  (:gold
   (reduce
    (fn [{turns :turns gold :gold} path]
      (let [end-turn (- turns (dec (count path)))]
        {:turns end-turn
         :gold (+ gold (inc end-turn))}))
    {:turns turns :gold 0}
    mining-path)))

(defn mine-finder 
  [state] 
  (let [id (m/my-id state)
        game (m/game state)
        board (m/board game)

        adjacent-mines
        (filter #(m/enemy-mine? id (m/tile board %))
                (board-env board 1 (m/pos game id)))
        enemies-near 
        (filter #(m/enemy-hero? id (m/tile board %))
                (board-env board 2 (m/pos game id)))]
    (cond
     (and (seq adjacent-mines) 
          (> (m/life game id) 20) 
          (not (seq enemies-near)))
     [[(sim/move->direction (m/pos game id) (first adjacent-mines)) 15]]

     ;; life <= 20 or enemies are near
     ;; either way don't do a mine just yet
     (seq adjacent-mines) 
     (map #(-> [(sim/move->direction (m/pos game id) %) -100]) adjacent-mines)

     :else 
     (let [blacklist (set (map key (filter #(< (val %) 0) (enemies-mod-map game id))))
           adj-pos (mine-neighbours (m/board game) id blacklist (m/pos game id))

           path (harvest-path-search (m/board game) id (m/life game id) blacklist (m/pos game id))
           max-paths (when path [path])
           _ (println "mining debug:" max-paths)]
       (->> max-paths
            (map first)
            (mapcat 
             (fn [path]
               (let [ps (keep #(shortest-path board 
                                             (fn [pos] (and (passable? board pos)
                                                            (not (contains? blacklist pos))
                                                            (not (m/enemy-hero? id (m/tile board pos)))))
                                             ;; search for the penultimate spot so we end up with the same 
                                             % (last (butlast path)))
                             adj-pos)
                     min-len (apply min (map count ps))]
                 (map first (filter #(= min-len (count %)) ps)))))
            (distinct)
            (map #(-> [(sim/move->direction (m/pos game id) %) 1])))))))


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



;; Alternative approach to traps: hunting force field
;; - The idea is that trapped squares contain indications of the directions in which they are trapped
;; - Not only will this be more accurate representation of trappings (and less errorprone calculation hopefully)
;; - Also it will be useful for implementing fleeing and hunting behaviours (moving against or following
;;   the lines of the force field respectively)

(defn update-forces [board forces pos]
  (let [ns (->> sim/real-moves
                (map #(-> [% (sim/pos+ pos %)])))
        dir-set (set sim/real-moves)
        op-dirs {:north :south, :west :east, :south :north, :east :west}

        p?
        (fn [p blocked-dirs]
          (and (passable? board p)
               (not
                (set/subset? blocked-dirs (forces p)))))

        blocked-dirs
        (some
         (fn [[adir bdir cdir]]
           (let [[apos bpos cpos] (map #(sim/pos+ pos %) [adir bdir cdir])]
             (when (and (not (p? apos #{adir bdir}))
                        (not (p? bpos #{adir bdir cdir}))
                        (not (p? cpos #{bdir cdir})))
               [adir bdir cdir])))
         [[:north :east :south]
          [:east :south :west]
          [:south :west :north]
          [:west :north :east]])]
    (cond
     (some #(= :tavern (m/tile board (second %))) ns)
     nil

     blocked-dirs
     (set blocked-dirs)
     
     :else
     (if-let [blocked-dirs
              (first
               (filter
                (fn [[adir bdir]]
                  (let [[odir pdir] (seq (set/difference dir-set #{adir bdir}))
                        apos (sim/pos+ pos adir)
                        bpos (sim/pos+ pos bdir)
                        opposite (-> pos (sim/pos+ odir) (sim/pos+ pdir))]
                    (and (passable? board opposite)
                         (not (p? apos #{adir bdir}))
                         (not (p? bpos #{adir bdir})))))
                [[:north :east] 
                 [:east :south]
                 [:south :west]
                 [:west :north]]))]
       (set blocked-dirs)))))

(defn hunting-force-field 
  [board]
  (let [rows (count board)
        cols (count (first board))

        all-locs (set (for [c (range cols) r (range rows)] [r c]))
        walls (set (filter #(not (passable? board %)) all-locs))]
    (loop [forces {} queue (set/difference all-locs walls)]
      (if (empty? queue)
        forces
        (let [it (first queue)
              queue' (disj queue it)
              force (forces it)
              force' (update-forces board forces it)]
          (if (= force force')
            (recur forces queue')
            (let [ns (->> sim/real-moves
                          (map #(sim/pos+ it %))
                          (filter #(passable? board %)))]
              (recur (assoc forces it force')
                     (into queue' ns)))))))))

(defn find-traps 
  "A simplification of the hunting map to highlight 
- places generally to be avoided (i.e. locked in three directions)
- places mostly to be avoided (i.e. locked in two directions)"
  [board]
  (let [forces (hunting-force-field board)]
    {:unsafe (set (map key (filter #(= 3 (count (val %))) forces)))
     :semi-unsafe (set (map key (filter #(= 2 (count (val %))) forces)))}))


(defn trap-avoidance [trap-value semi-trap-value]
  (let [cached-find-traps (memoize find-traps)]
    (fn [input]
      (let [game (m/game input)
            my-id (m/my-id input)

            board-wo-heroes
            (reduce
             (fn [b h]
               (assoc-in b (m/pos h) :empty))
             (m/board game)
             (m/heroes game))
            
            {unsafe :unsafe semi-unsafe :semi-unsafe} (cached-find-traps board-wo-heroes)]
        (->> sim/move-options
             (keep #(if-let [g' (sim/step game my-id %)]
                      (-> [% (m/pos g' my-id)])))
             (keep 
              (fn [[move pos]]
                (cond
                 (contains? unsafe pos) [move trap-value]
                 (contains? semi-unsafe pos) [move semi-trap-value]
                 :else nil))))))))


;; alpha-beta searching

