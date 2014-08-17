(ns vindinium.core
  (:gen-class)
  (:require [clj-http.client :as http]
            [vindinium.model :as m])
  (:use [clojure.pprint :only [pprint]]
        [clojure.string :only [split]]
        [clojure.java.io :only [writer file]]
        [slingshot.slingshot :only [try+, throw+]]
        [clojure.core.match :only (match)]
        vindinium.bots))

(def server-url "http://vindinium.org")
(def secret-key (slurp "secret.key"))

;; Bots

(defn bot 
  "Creates a safe bot function that times the behaviour function"
  [behaviour]
  (fn [input]
    (try (let [start (System/currentTimeMillis) 
               fut (future (behaviour input))
               res (deref fut 1000 :timeout)
               end (System/currentTimeMillis)]
           (println (str "Agent took " (- end start) "ms"))
           (if (= res :timeout)
             (do 
               (println "Bot timeout")
               (future-cancel fut)
               (rand-move))
             (name res)))
         (catch Exception e
           (println "Bot exception" e)
           (.printStackTrace e)
           (rand-move)))))

(def default-bot (fn [_] (rand-move)))

(def current-bot 
  (bot (heuristic-agent
        (with-meta (autist-dfs-agent 7) {:label "autist-dfs" :weight 1})
        (with-meta (tavern-finder 30) {:label "tavern-finder" :weight 0.9})
        (with-meta (mine-finder 30) {:label "mine-finder" :weight 1})
        (with-meta hero-one-oh-one {:label "combat" :weight 300})
        (with-meta (avoid-spawning-spots 0.1) {:label "spawn-avoider" :weight 0.1}))))

;; Server interaction

(defn parse-tile [tile]
  (match (vec tile)
         [\space \space] :empty
         [\# \#] :wall
         [\[ \]] :tavern
         [\$ \-] [:mine nil]
         [\$ i] [:mine (- (int i) 48)]
         [\@ i] [:hero (- (int i) 48)]))

(defn parse-tiles [size tiles] 
  (->> (seq tiles)
       (partition 2)
       (map parse-tile)
       (partition size)
       (map vec)
       (vec)))

(defn parse-input [input] 
  (-> input
      (update-in [:game :board] 
                 (fn [board]
                   (parse-tiles (:size board) (:tiles board))))
      (update-in [:game :heroes] (fn [heros]
                                   (zipmap
                                    (map :id heros)
                                    (map 
                                     #(assoc % :pos [(:x (:pos %)) (:y (:pos %))]
                                             :spawnPos [(:x (:spawnPos %)) (:y (:spawnPos %))])
                                     heros))))))

(defn request [url params]
  (try+
   (let [body (:body (http/post url {:form-params params :as :json}))
         input (parse-input body)]
     #_(println (pr-str input))
     input)
    (catch map? {:keys [status body]}
      (println (str "[" status "] " body))
      (throw+))))

(defn step [logger bot from]
  (loop [input from]
    (binding [*out* logger] (println (pr-str input))) 
    (println "Turn" (m/turn (m/game input)))
    (let [next (request (:playUrl input) {:dir (bot input)})]
      (if (:finished (:game next)) (println "") (recur next)))))

(defn training 
  ([secret-key turns]
     (training secret-key turns current-bot))
  ([secret-key turns bot]
     (let [input (request (str server-url "/api/training") {:key secret-key :turns turns})]
       (println (str "Starting training game " (:viewUrl input)))
       (with-open [os (writer (file "training" (str (last (split (:viewUrl input) #"/")) ".txt")))]
         (step os bot input))
       (println (str "Finished training game " (:viewUrl input))))))

(defn arena 
  ([secret-key games]
     (arena secret-key games current-bot))
  ([secret-key games bot]
     (loop [it 1]
       (let [p #(println (str "[" it "/" games "] " %))
             _ (p "Waiting for pairing...")
             input (request (str server-url "/api/arena") {:key secret-key})]
         (p (str "Starting arena game " (:viewUrl input)))
         (try (with-open [os (writer (file "arena" (str (last (split (:viewUrl input) #"/")) ".txt")))]
                (step os bot input))
              (catch Exception e
                (p (str "Errored out: " e))
                (.printStackTrace e)))
         (p (str "Finished arena game " (:viewUrl input)))
         (when (< it games) (recur (+ it 1)))))))


;; Main

(defn -main [& args]
  (match (vec args)
         ["training" nb] (training secret-key (Integer/parseInt nb))
         ["arena" nb] (arena secret-key (Integer/parseInt nb))))


