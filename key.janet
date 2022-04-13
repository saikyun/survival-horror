(use freja/flow)
(import ./state :as s)
(import ./tau-is-180-degrees :as tau)

(defn tick
  [k])

(defn render
  [{:pos pos
    :rot rot}]
  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef ;pos 0)
    (rl-rotatef (tau/->deg rot) 0 0 1)
    (draw-texture-rec s/items s/key-source [-16 -16] :white)))

(defn grab
  [k grabber]
  (:pick-up grabber k))

(defn new
  [data]
  (merge-into
    @{:rot 0
      :pos @[0 0]
      :grab |(grab $0 $1)
      :tick |(tick $)
      :render |(render $)}
    data))
