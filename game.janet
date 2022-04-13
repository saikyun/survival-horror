(use freja/flow)
(import ./input)
(import ./state)
(import ./human)
(import ./tau-is-180-degrees :as tau)
(import ./key)

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
  (set state/items (load-texture "assets/items0000.png"))

  (merge-into state/player (human/new
                             @{:speed 2
                               :back-width 0.2
                               :pos @[50 50]

                               :right-arm @{:shoulder-pos @[0 0]
                                            :shoulder-offset [2 8]
                                            :wrist-pos @[0 0]
                                            :upper-arm-length 16
                                            :lower-arm-length 16}}))

  (array/clear state/gos)
  (array/push state/gos state/player)
  (array/push state/gos (key/new @{:pos @[10 10]})))

(defn lock-mouse
  ``
  For some reason has a clear bias toward the left, not sure why.
  Should probably change to relative positions / deltas for :target.
  ``
  [render-pos]
  '(let [{:target target
          :pos pos
          :right-arm {:upper-arm-length upper-arm-length
                      :lower-arm-length lower-arm-length
                      :shoulder-offset shoulder-offset}} state/player
         reach-dir (v/v- target pos)
         reach-mag (min (v/mag reach-dir)
                        (+ lower-arm-length upper-arm-length))
         new-pos (-> (v/normalize reach-dir)
                     (v/v* reach-mag))
         abs-pos (-> new-pos
                     (v/v+ pos)
                     (v/v* state/render-scale)
                     (v/v+ render-pos))
         new-mp (map math/round abs-pos)]
     (put state/player :target (v/v+ pos new-pos))
     (hide-cursor)
     (put state/player :mouse-diff (v/v- abs-pos new-mp))
     (set-mouse-position ;new-mp)))

### rendering

(def layers
  @[@[]
    @[]
    @[]])

(defn render
  [el]
  (clear-background (map |(/ $ 255) [24 10 10]))

  (loop [l :in layers]
    (array/clear l))

  (loop [go :in state/gos]
    (:tick go)
    (update layers (+ 1 (get go :layer 0)) array/push go))

  (loop [l :in layers
         go :in l]
    (:render go))

  '(loop [go :in state/gos]
     (:render go))

  (if (el :focused?)
    (lock-mouse [(el :render-x) (el :render-y)])
    (show-cursor)))

(start-game {:render render
             :on-event |(input/on-event $0 $1)
             :init init
             :scale state/render-scale})
