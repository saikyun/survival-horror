(use freja/flow)
(import ./state :as s)
(import ./tau-is-180-degrees :as tau)

(defn tick
  [k]
  (update k :pos v/v+ (v/v* (k :vel) (get-frame-time)))
  (update k :vel v/v* 0.95))

(defn render
  [{:pos pos
    :rot rot}]
  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef ;pos 0)
    (rl-rotatef (tau/->deg rot) 0 0 1)
    (draw-texture-rec s/items s/key-source [-16 -16] :white)))

(defn can-grab?
  [{:pos pos
    :radius radius}
   grab-pos]
  (let [d (v/dist pos grab-pos)]
    (<= d radius)))

(defn grab
  [k grabber]
  (:pick-up grabber k))

(defn new
  [data]
  (merge-into
    @{:rot 0
      :radius 5
      :pos @[0 0]
      :vel @[0 0]
      :can-grab? |(can-grab? $0 $1)
      :grab |(grab $0 $1)
      :tick |(tick $)
      :render |(render $)}
    data))
