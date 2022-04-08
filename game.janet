(use freja/flow)
(import ./input)
(import ./state)
(import ./human)
(import ./tau-is-180-degrees :as tau)

### initialization

(defn init
  []
  (set state/legs (load-texture "assets/legs0000.png"))
  (set state/body (load-texture "assets/body0000.png"))
  (set state/head (load-texture "assets/head0000.png"))

  (merge-into state/player
              @{:target @[0 0]
                :speed 2
                :back-width 0.2
                :in @[0 0]
                :walk-angle 0
                :legs-target-angle 0
                :pos @[50 50]
                :angles @{:head 0
                          :body 0
                          :legs 0}
                :tick |(human/tick $)
                :render |(human/render $)}))

### rendering

(defn render
  [el]
  (clear-background (map |(/ $ 255) [24 10 10]))
  (loop [go :in state/gos]
    (:tick go))

  (loop [go :in state/gos]
    (:render go)))

(start-game {:render render
             :on-event input/on-event
             :init init
             :scale 3})
