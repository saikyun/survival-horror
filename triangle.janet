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
  Order of side1 and side2 should not matter.
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

(defn refresh-arm
  ``
  Takes table representing an arm.
  Keys:
  :shoulder-pos / :wrist-pos is [x y]
  :upper-length / :lower-length is the length of the upper and lower arm
  :left? is truthy if it is a left arm
  ``
  [arm]
  (let [{:shoulder-pos start
         :wrist-pos stop
         :upper-arm-length upper-l
         :lower-arm-length lower-l
         :left? left?} arm
        reach-vector (v/v- stop start)
        reach-dist (max (min (+ upper-l lower-l)
                             (v/mag reach-vector))
                        (math/abs (- upper-l lower-l)))
        reach-dir (v/normalize reach-vector)
        reach-a (tau/atan2xy ;reach-dir)

        # angle from shoulder -> elbow
        upper-a (-> (sides->angle reach-dist upper-l lower-l)
                    (* (if left?
                         -1
                         1))
                    (+ reach-a))

        elbow-pos (v/v* (tau/inverse-atan upper-a)
                        upper-l)

        # angle from elbow -> wrist
        lower-a (-> (sides->angle reach-dist lower-l upper-l)
                    (* (if left?
                         1
                         -1))
                    (+ reach-a))

        wrist-pos (-> (tau/inverse-atan lower-a)
                      (v/v* lower-l)
                      (v/v+ elbow-pos))]

    (-> arm
        (put :reach-distance reach-dist) # distance between shoulder and wrist
        (put :reach-angle reach-a) # angle between shoulder and wrist
        (put :upper-arm-angle upper-a)
        (put :elbow-pos elbow-pos)
        (put :lower-arm-angle lower-a)
        (put :wrist-pos wrist-pos))))

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
  (refresh-arm arm)

  (draw-line-ex (arm :shoulder-pos) (arm :elbow-pos) 3 :blue)
  (draw-line-ex (arm :elbow-pos) (arm :wrist-pos) 2 :green)

  (put arm :left? true)
  (refresh-arm arm)

  (draw-line-ex (arm :shoulder-pos) (arm :elbow-pos) 3 :blue)
  (draw-line-ex (arm :elbow-pos) (arm :wrist-pos) 2 :green))

(defn on-event
  [_ ev]
  (match ev
    {:mouse/pos p}
    (set mp p)))

(start-game {:render render
             :scale 3
             :on-event on-event})
