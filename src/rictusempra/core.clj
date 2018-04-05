(ns rictusempra.core
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]))

(defn replay-id
  [replay]
  (get-in replay [:header :body :properties :value :id :value :str]))

(defn objects
  [replay]
  (get-in replay [:content :body :objects]))

(defn search-objects
  "Apropos-style search of objects in this replay"
  [replay regex]
  (filter #(re-seq regex %) (objects replay)))

(defn read-replay
  [f]
  (-> f
      slurp
      (json/read-str :key-fn keyword)))

(defn stream-id
  [replay class-name]
  (->> replay
       (#(get-in % [:content :body :class_mappings]))
       (filter #(= class-name (:name %)))
       first
       :stream_id))

(defn frames
  [replay]
  (get-in replay [:content :body :frames]))

(defn nth-frame
  [replay n]
  (nth (frames replay) n))

(defn frames-in-window
  [replay start-time end-time]
  (let [within-range? #(< start-time (:time %) end-time)]
    (filter within-range? (frames replay))))

(defn replications
  [replay]
  (mapcat :replications (frames replay)))

(defn replications-for-actor
  [replay actor-id]
  (let [reps (replications replay)
        actor-to-reps (group-by #(-> % :actor_id :value) reps)]
    (get actor-to-reps actor-id)))

(defn replications-of-class
  [replay class-name]
  (filter #(= )))

(defn positions-for-actor
  [replay actor-id]
  (let [reps (replications-for-actor replay actor-id)]
    (map #(get-in % [:value :updated 0 :value :rigid_body_state :location])
         (drop 1 reps))))

(defn actors
  [replay]
  (let [reps (replications replay)
        spawned-class #(-> % :value :spawned :class_name)
        spawn-reps (filter spawned-class reps)]
    (group-by #(-> % :actor_id :value) spawn-reps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Replay simplification
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ball-class "TAGame.Ball_TA")
(def car-class "TAGame.Car_TA")
(def boost-class "TAGame.CarComponent_Boost_TA")
(def rigid-body-state-class "TAGame.RBActor_TA:ReplicatedRBState")
(def player-info-class "Engine.Pawn:PlayerReplicationInfo")
(def seconds-remaining-class "TAGame.GameEvent_Soccar_TA:SecondsRemaining")
(def explosion-data-class "TAGame.Ball_TA:ReplicatedExplosionDataExtended")
(def hit-team-class "TAGame.Ball_TA:HitTeamNum")

(def classes-of-interest
  #{ball-class
    car-class
    rigid-body-state-class})

(defn unpack-replication-value
  [replication]
  (merge (dissoc replication :value) (:value replication)))

(defn replication-type
  [replication]
  (-> replication :value keys first))

(defmulti simplify-replication replication-type)

(defmethod simplify-replication
  :spawned
  [{:keys [value] :as replication}]
  (let [spawned (select-keys (:spawned value) [:class_name :initialization])]
    (when (classes-of-interest (:class_name spawned))
      (-> replication
          (dissoc :value)
          (assoc :spawned spawned)))))

(defmethod simplify-replication
  :updated
  [{:keys [value] :as replication}]
  (let [updated (filter (comp classes-of-interest :name) (:updated value))]
    (when (seq updated)
      (-> replication
          (dissoc :value)
          (assoc :updated (mapv :value updated))))))

(defmethod simplify-replication
  :destroyed
  [replication]
  (assoc-in replication [:value :destroyed] true))

(defmethod simplify-replication
  :default
  [replication]
  replication)

(def simplify-replication-xf
  (comp
   (map #(update % :actor_id :value))
   (keep simplify-replication)
   (map unpack-replication-value)))

(defn simplify-replications
  [replications]
  (transduce simplify-replication-xf conj replications))

(defn simplify-frame
  [frame]
  (update frame :replications simplify-replications))

(defn simplify-replay
  [replay]
  (map simplify-frame (frames replay)))

(defn simplify-file
  [f outputf]
  (let [replay (read-replay f)
        simplified (simplify-replay replay)]
    (binding [*print-length* false]
      (spit outputf (pr-str simplified)))))

(comment
  (simplify-file "./resources/landmarks-calvin.json" "./resources/landmarks-calvin.edn")
  (def replay (simplify-replay (read-replay "./resources/landmarks-calvin.json")))

  )
