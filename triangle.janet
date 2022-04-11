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

(defn left-arm
  [{:start start
    :stop stop
    :upper-length upper-l
    :lower-length lower-l}]
  (let [reach-vector (v/v- stop start)
        reach-dist (max (min (+ upper-l lower-l)
                             (v/mag reach-vector))
                        (math/abs (- upper-l lower-l)))
        reach-dir (v/normalize reach-vector)
        reach-a (tau/atan2 (reach-dir 1) (reach-dir 0))

        upper-a (tau/acos
                  (/ (+ (* reach-dist reach-dist)
                        (* upper-l upper-l)
                        (- (* lower-l lower-l)))
                     (* 2 reach-dist upper-l)))

        end (v/v+ [0 0] (v/v* reach-dir reach-dist))

        upper-end (v/v*
                    [(tau/cos upper-a)
                     (tau/sin upper-a)]
                    upper-l)

        lower-a (tau/acos
                  (/ (+ (* reach-dist reach-dist)
                        (* lower-l lower-l)
                        (- (* upper-l upper-l)))
                     (* 2 reach-dist lower-l)))

        lower-end (v/v+ upper-end (v/v*
                                    [(tau/cos lower-a)
                                     (- (tau/sin lower-a))]
                                    lower-l))]

    # gonna rotate triangle

    #(printf "lower-a: %p" lower-a)

    #(rl-rotatef (* 180 (/ (tau/atan2 (dir 1) (dir 0)) math/pi)) 0 0 1)
    (draw-line-ex [0 0] upper-end 3 :blue)
    (draw-line-ex upper-end lower-end 2 :green)
    #(draw-rectangle ;(map math/floor end) 10 20 :brown)
))

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

  (left-arm {:start origin
             :stop mp
             :upper-length 50
             :lower-length 40}))

(defn on-event
  [_ ev]
  (match ev
    {:mouse/pos p}
    (set mp p)))

(start-game {:render render
             :scale 3
             :on-event on-event})
