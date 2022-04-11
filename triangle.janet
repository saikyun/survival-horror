(use freja/flow)
(import ./tau-is-180-degrees :as tau)

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

(defn sides->angle
  ``
  Takes triangle side lengths side1-3.
  Uses "Law of cosines".
  Returns the angle opposite of side3.
  
    A  <-- returned angle
  1/ \2
  /___\
    3
  ``
  [side1 side2 side3]
  (tau/acos
    (/ (+ (* side1 side1)
          (* side2 side2)
          (- (* side3 side3)))
       (* 2 side1 side2))))

(defn left-arm
  ``
  Takes table representing an arm.
  start / stop is [x y]
  upper-length / lower-length is the length of the upper and lower arm.

  
  ``
  [arm]
  (let [{:shoulder-pos start
         :wrist-pos stop
         :upper-arm-length upper-l
         :lower-arm-length lower-l} arm
        reach-vector (v/v- stop start)
        reach-dist (max (min (+ upper-l lower-l)
                             (v/mag reach-vector))
                        (math/abs (- upper-l lower-l)))
        reach-dir (v/normalize reach-vector)
        reach-a (tau/atan2xy ;reach-dir)

        upper-a (-> (sides->angle reach-dist upper-l lower-l)
                    (+ reach-a))

        end (v/v+ [0 0] (v/v* reach-dir reach-dist))

        elbow-pos (v/v*
                    [(tau/cos upper-a)
                     (tau/sin upper-a)]
                    upper-l)

        lower-a (-> (sides->angle reach-dist lower-l upper-l)
                    (- reach-a))

        wrist-pos (v/v+ elbow-pos
                        (v/v* [(tau/cos lower-a)
                               (- (tau/sin lower-a))]
                              lower-l))]
    (-> arm
        (put :reach-distance reach-dist) # distance between shoulder and wrist
        (put :reach-angle reach-a) # angle between shoulder and wrist
        (put :upper-arm-angle upper-a)
        (put :elbow-pos elbow-pos)
        (put :lower-arm-angle lower-a)
        (put :wrist-pos wrist-pos))

    (draw-line-ex (arm :shoulder-pos) (arm :elbow-pos) 3 :blue)
    (draw-line-ex (arm :elbow-pos) (arm :wrist-pos) 2 :green)))

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

  (left-arm @{:shoulder-pos origin
              :wrist-pos mp
              :upper-arm-length 50
              :lower-arm-length 40}))

(defn on-event
  [_ ev]
  (match ev
    {:mouse/pos p}
    (set mp p)))

(start-game {:render render
             :scale 3
             :on-event on-event})
