(use freja/flow)
(import ./state :as s)
(import ./math :as m)
(import ./tau-is-180-degrees :as tau)

(defn tick
  [human]
  (put human :target-angle (tau/atan2 ;(v/v- (human :target) (human :pos))))
  (unless (m/v-zero? (human :in))
    (let [a (tau/atan2 ;(human :in))]
      (put human :walk-angle a)))

  (put human :legs-target-angle (human :walk-angle))
  (def back-width 0.2)
  (def lookaway (tau/shortest-angle (human :target-angle)
                                    (human :walk-angle)))

  (def trying-to-move (m/v-zero? (human :in)))

  (when (and (not trying-to-move)
             (> (math/abs lookaway) (- 1 0.5)))
    (update human :legs-target-angle (fn [a] (tau/normalize (- a 1)))))

  (update human :pos v/v+ (-> (v/normalize (human :in))
                              (v/v* (* (m/lerp 0.3 1 (- 1 (math/abs lookaway)))
                                       (human :speed)))))

  (when (not trying-to-move)
    (let [{:legs-target-angle ta
           :pos pos
           :angles {:legs la
                    :body ba}} human

          move-angle (tau/shortest-angle la ta)
          move-sector [la (+ la move-angle)]

          start (- (- ba 1) back-width)
          back-angle (tau/shortest-angle start (+ start back-width back-width))
          back-sector [start (+ start back-width back-width)]

          overlap (tau/sector-overlap move-sector back-sector)]

      '(defer (rl-pop-matrix)
         (rl-push-matrix)
         (rl-scalef 0.3333 0.3333 1)
         (draw-text (string/format "back: %.02f" ba) [10 35] :color :white)
         (draw-text (string/format "back-sector: %.02f %.02f" ;back-sector) [10 60] :color :white))

      '(defer (rl-pop-matrix)
         (rl-push-matrix)
         (rl-translatef ;pos 0)
         (tau/draw-circle-sector
           [0 0]
           100
           ;move-sector
           10
           :blue)
         (tau/draw-circle-sector
           [0 0]
           80
           ;back-sector
           10
           :red))

      '(when overlap
         (draw-text "OUCH" [10 100] :color :white))

      (put-in
        human
        [:angles :legs]
        (-> move-angle
            (* (if overlap -0.05 0.05))
            (+ la)
            tau/normalize))))

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

  (let [{:body ba
         :head ha
         :legs la} (human :angles)
        h-diff 0.7
        diff 0.6
        new-body-angle ba
        a '(-> (tau/angle-clamp (- la diff) (+ la diff) ba)
               #(tau/normalize)
)
        new-head-angle
        (m/lerp
          ha
          (+ ha
             (tau/shortest-angle
               ha
               (tau/angle-clamp (- la h-diff) (+ la h-diff) ha)))
          0.1)

        body-diff (tau/shortest-angle la ba)]

    (when (> (math/abs body-diff) diff)
      (update-in human [:angles :legs]
                 + (* 0.03 body-diff)))

    (put-in human [:angles :head] (tau/normalize new-head-angle))

    (put-in human [:angles :body] (tau/normalize new-body-angle))))


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
