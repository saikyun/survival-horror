(import ../joint)

(use freja/flow)
(import ../tau-is-180-degrees :as tau)

(var mp @[0 0])

(defn rng
  [start stop]
  (+ start (* (math/random) (- stop start))))

(def gos (seq [_ :range [0 0]]
           @{:pos [(rng 0 300)
                   (rng 0 300)]
             :radius (rng 10 20)
             :color [(rng 0.2 0.8)
                     (rng 0.2 0.8)
                     (rng 0.2 0.8)]}))

(def arm
  @{:shoulder-pos [0 0]
    :wrist-pos [0 0]
    :upper-arm-length 50
    :lower-arm-length 40})

(defn render
  [{:width rw :height rh}]
  (def origin [100 100])

  (loop [go :in gos
         :let [{:pos p
                :radius r
                :color c} go]]
    (draw-circle-v p r c)
    (when (check-collision-circles p r mp 5)
      (update go :pos v/v+ (v/normalize (v/v- p mp)))))

  (rl-translatef ;origin 0)

  (put arm :wrist-pos (v/v- mp origin))
  (put arm :left? false)
  (joint/refresh-arm arm)

  (draw-line-ex (arm :shoulder-pos) (arm :elbow-pos) 3 :blue)
  (draw-line-ex (arm :elbow-pos) (arm :wrist-pos) 2 :green)

  (put arm :left? true)
  (joint/refresh-arm arm)

  (draw-line-ex (arm :shoulder-pos) (arm :elbow-pos) 3 :blue)
  (draw-line-ex (arm :elbow-pos) (arm :wrist-pos) 2 :green))

(defn on-event
  [_ ev]
  (match ev
    {:mouse/pos p}
    (set mp p)))

(start-game {:on-event on-event
             :scale 3
             :render render})
