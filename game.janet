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
  (set state/upper-arm (load-texture "assets/upper-arm0000.png"))
  (set state/lower-arm (load-texture "assets/lower-arm0000.png"))
  (set state/hand (load-texture "assets/hand0000.png"))
  (set state/right-upper-arm (load-texture "assets/right-upper-arm0000.png"))
  (set state/right-lower-arm (load-texture "assets/right-lower-arm0000.png"))
  (set state/right-hand (load-texture "assets/right-hand0000.png"))
  (set state/right-hand-closed (load-texture "assets/right-hand-closed0000.png"))

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
                :render |(human/render $)

                :right-arm @{:shoulder-pos @[0 0]
                             :shoulder-offset [1 8]
                             :wrist-pos @[0 0]
                             :upper-arm-length 16
                             :lower-arm-length 21}}))

### rendering

(var t 0)

(defn render
  [el]
  (+= t (get-frame-time))

  (clear-background (map |(/ $ 255) [24 10 10]))
  (loop [go :in state/gos]
    (:tick go))

  (loop [go :in state/gos]
    (:render go))

  (def x (-> t
             tau/sin
             (* 100)
             math/floor))
  #(draw-circle (+ 200 x) 10 10 :white)
  )

(start-game {:render render
             :on-event input/on-event
             :init init
             :scale 3})
