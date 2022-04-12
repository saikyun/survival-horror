(import freja/vector-math :as v)
(import ./tau-is-180-degrees :as tau)

(defn refresh-arm
  ``
  Takes table representing an arm.
  Keys:
  :shoulder-pos / :wrist-pos is [x y]
  :upper-length / :lower-length is the length of the upper and lower arm
  :left? is truthy if it is a left arm

  Check ./examples/joint.janet for example usage.
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
        upper-a (-> (tau/sides->angle reach-dist upper-l lower-l)
                    (* (if left?
                         -1
                         1))
                    (+ reach-a))

        elbow-pos (-> (v/v* (tau/inverse-atan upper-a)
                            upper-l)
                      (v/v+ start))

        # angle from elbow -> wrist
        lower-a (-> (tau/sides->angle reach-dist lower-l upper-l)
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
