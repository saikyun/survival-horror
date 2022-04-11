(use freja/flow)
(import ./state :as s)
(import ./math :as m)
(import ./joint)
(import ./tau-is-180-degrees :as tau)

(defn set-target-angles
  [human]
  (let [{:in in
         :target target
         :pos pos} human]
    (put human :target-angle (tau/atan2 ;(v/v- target pos)))

    (unless (m/v-zero? in)
      (let [a (tau/atan2 ;in)]
        (put human :walk-angle a)
        (put human :legs-target-angle (human :walk-angle))))

    (put human :lookaway
         (tau/shortest-angle (human :target-angle)
                             (human :walk-angle)))))

(defn rotate-body-parts
  [human]
  (def trying-to-move (m/v-zero? (human :in)))

  (update-in human [:angles :head]
             (fn [a]
               (-> (+ a (* 0.5 (tau/shortest-angle a (human :target-angle))))
                   tau/normalize)))

  (update-in human [:angles :body]
             (fn [a]
               (-> (+ a (* 0.1 (tau/shortest-angle
                                 a
                                 (+ (/ (- (* 12 #(math/random)
                                             1) 6) 180)
                                    (human :target-angle)))))
                   tau/normalize)))

  (when (not trying-to-move)
    (let [{:legs-target-angle ta
           :pos pos
           :back-width back-width
           :angles {:legs la
                    :body ba}} human

          move-angle (tau/shortest-angle la ta)
          move-sector [la (+ la move-angle)]

          start (- (- ba 1) back-width)
          back-angle (tau/shortest-angle start (+ start back-width back-width))
          back-sector [start (+ start back-width back-width)]

          overlap (tau/sector-overlap move-sector back-sector)]

      (put-in
        human
        [:angles :legs]
        (-> move-angle
            (* (if overlap -0.1 0.1))
            (+ la)
            tau/normalize))))

  (let [{:body ba
         :head ha
         :legs la} (human :angles)
        h-diff 0.7
        diff 0.6

        new-body-angle
        (+ ba
           (m/lerp 0
                   (tau/shortest-angle
                     ba
                     (tau/angle-clamp (- la diff) (+ la diff) ba))
                   0.1))

        new-head-angle
        (+ ha
           (m/lerp
             0
             (tau/shortest-angle
               ha
               (tau/angle-clamp (- la h-diff) (+ la h-diff) ha))
             0.1))

        body-diff (tau/shortest-angle la ba)]

    (when (> (math/abs body-diff) diff)
      (update-in human [:angles :legs]
                 + (* 0.03 body-diff)))

    (put-in human [:angles :head] (tau/normalize new-head-angle))

    (put-in human [:angles :body] (tau/normalize new-body-angle))))

(defn move
  [human]
  (let [{:lookaway lookaway
         :in in
         :speed speed} human
        slowdown (m/lerp 0.3 1 (- 1 (math/abs lookaway)))
        delta (-> (v/normalize in)
                  (v/v* (* slowdown speed)))]
    (update human :pos v/v+ delta)))

(defn tick
  [human]
  (set-target-angles human)

  (def trying-to-move (m/v-zero? (human :in)))

  (when (and (not trying-to-move)
             (> (math/abs (human :lookaway)) (- 1 0.5)))
    (update human :legs-target-angle (fn [a] (tau/normalize (- a 1)))))

  (move human)

  (rotate-body-parts human))

(defn render
  [human]
  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef ;(human :pos) 0)

    (def angles (human :angles))

    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (tau/->deg (angles :legs)) 0 0 1)
      (draw-texture s/legs -5 -15 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (tau/->deg (angles :body)) 0 0 1)
      (draw-texture s/body -5 -13 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (tau/->deg (angles :head)) 0 0 1)
      (draw-texture s/head -5 -9 :white))
    #(draw-circle 0 0 1 :white)
))
