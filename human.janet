(use freja/flow)
(import ./state :as s)
(import ./math :as m)
(import ./joint)
(import bounded-queue :as queue)
(import ./tau-is-180-degrees :as tau)

### do stuff

(defn set-target-angles
  [human]
  (let [{:in in
         :target target
         :pos pos} human]
    (put human :target-angle (tau/atan2xy ;target))

    (unless (m/v-zero? in)
      (let [a (tau/atan2xy ;in)]
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

  (unless (mouse-button-down? 1)
    (update-in human [:angles :body]
               (fn [a]
                 (-> (+ a (* 0.1 (tau/shortest-angle
                                   a
                                   (+ (/ (- (* 12 #(math/random)
                                               1) 6) 180)
                                      (human :target-angle)))))
                     tau/normalize))))

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
        diff 0.2

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

(defn arm-length
  [{:right-arm {:upper-arm-length u
                :lower-arm-length l}}]
  (+ u l))

(defn move
  [human]
  (let [{:lookaway lookaway
         :in in
         :speed speed} human
        slowdown (m/lerp 0.3 1 (- 1 (math/abs lookaway)))
        delta (-> (v/normalize in)
                  (v/v* (* slowdown speed)))
        target-dist (- (v/mag (human :target))
                       (arm-length human))
        delta (if (pos? target-dist)
                (v/v+ delta
                      (v/v* (v/normalize (human :target))
                            (* (get-frame-time) target-dist)))
                delta)]
    (update human :pos v/v+ delta)))

(defn pick-up
  [human item]
  (put human :holding item)
  (put item :layer -1))

(defn release-item
  [human]
  (let [item (human :holding)]
    # calculates average velocity of hand last few frames
    (var vel @[0 0])
    (var i 0)
    (loop [[x y] :iterate (queue/pop (human :last-hand-vels))]
      (update vel 0 + x)
      (update vel 1 + y)
      (++ i))
    (update vel 0 / i)
    (update vel 1 / i)
    (put item :vel vel)
    (put human :holding nil)
    (put item :layer nil)))

(defn hand-pos-abs
  [{:pos pos :right-arm {:wrist-pos wrist-pos}}]
  (v/v+ pos wrist-pos))

(defn tick
  [human]
  (set-target-angles human)

  (def trying-to-move (m/v-zero? (human :in)))

  (when (and (not trying-to-move)
             (> (math/abs (human :lookaway)) (- 1 0.5)))
    (update human :legs-target-angle (fn [a] (tau/normalize (- a 1)))))

  (def delta (move human))

  (rotate-body-parts human)

  (let [arm (human :right-arm)]
    (put arm :shoulder-pos
         (v/v* (-> (+ (tau/atan2xy ;(arm :shoulder-offset))
                      (get-in human [:angles :body]))
                   tau/inverse-atan)
               (v/mag (arm :shoulder-offset))))

    (let [target-hand-dir (-> (human :target)
                              (v/v- (arm :wrist-pos)))
          hand-acc (v/v* target-hand-dir 20)
          hand-acc (if (< 0.5 (math/abs (tau/vector-shortest-angle ;hand-acc
                                                                   ;(human :hand-vel))))
                     (v/v* hand-acc 10)
                     hand-acc)]
      (update human :hand-vel v/v+ hand-acc)
      (update human :hand-vel v/v* 0.5)
      (update human :hand-vel (fn [hv]
                                (let [mag (v/mag hv)
                                      max-vel 300
                                      n (v/normalize hv)]
                                  (if (> mag max-vel)
                                    (v/v* n max-vel)
                                    hv)))))

    (update arm :wrist-pos v/v+ (v/v* (human :hand-vel) (get-frame-time)))

    (joint/refresh-arm arm))

  (if (human :grabbing)
    (queue/push (human :last-hand-vels) (human :hand-vel))
    (put human :grab-time 0))

  (put human :target (get-in human [:right-arm :wrist-pos]))

  (cond (human :grabbing)
    (do (update human :grab-time + (get-frame-time))
      (var to-grab nil)
      (var almost-grab nil)
      (loop [go :in s/gos
             :when (go :grab)]
        (when (:can-grab? go
                          (hand-pos-abs human))
          (set to-grab go))

        (when (>= 20 (v/dist (go :pos) (hand-pos-abs human)))
          (set almost-grab go)))
      (cond to-grab
        (:grab to-grab human)

        almost-grab
        (update human :target
                m/v-lerp
                (v/v- (almost-grab :pos) (human :pos))
                (* 0.7 (min 1 (human :grab-time))))))

    (and (not (human :grabbing))
         (human :holding))
    (release-item human))

  (when-let [i (human :holding)]
    (put i :rot (+ 0.1 (get-in human [:right-arm :lower-arm-angle])))
    (put i :pos (v/v+ (human :pos)
                      (get-in human [:right-arm :wrist-pos])))))

### render

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

    (let [{:shoulder-pos shoulder
           :upper-arm-angle upper-arm-angle
           :lower-arm-angle lower-arm-angle
           :elbow-pos elbow
           :wrist-pos wrist} (human :right-arm)]

      (draw-line-ex shoulder elbow 3 :green)
      (draw-line-ex elbow wrist 2 :blue)

      (defer (rl-pop-matrix)
        (rl-push-matrix)
        (rl-translatef ;shoulder 0)
        (rl-rotatef (-> (+ upper-arm-angle # (angles :body)
)
                        tau/->deg) 0 0 1)
        (draw-texture s/right-upper-arm -5 -4 :white))

      (defer (rl-pop-matrix)
        (rl-push-matrix)
        (rl-translatef ;elbow 0)
        (rl-rotatef (-> (+ lower-arm-angle # (angles :body)
)
                        tau/->deg) 0 0 1)
        (draw-texture s/right-lower-arm -2 -3 :white))

      (defer (rl-pop-matrix)
        (rl-push-matrix)
        (rl-translatef ;wrist 0)
        (rl-rotatef (-> (+ lower-arm-angle #(angles :body)
)
                        tau/->deg) 0 0 1)

        (draw-texture (if (human :holding)
                        s/right-hand-closed
                        s/right-hand)
                      -4 -5 :white)))

    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (tau/->deg (angles :head)) 0 0 1)
      (draw-texture s/head -5 -9 :white))))

(defn new
  [data]
  (merge-into
    @{:target @[0 0]
      :in @[0 0]
      :mouse-diff @[0 0]
      :walk-angle 0
      :grab-time 0
      :last-hand-vels (queue/new 5)
      :hand-vel @[0 0]
      :legs-target-angle 0
      :angles @{:head 0
                :body 0
                :legs 0}
      :tick |(tick $)
      :render |(render $)
      :pick-up |(pick-up $0 $1)}
    data))
