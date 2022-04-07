(use freja/flow)

(var legs nil)
(var body nil)
(var head nil)

# I need the distance between two angles
# angles should be in radians, like math/atan2

(defn init
  []
  (set legs (load-texture "assets/legs0000.png"))
  (set body (load-texture "assets/body0000.png"))
  (set head (load-texture "assets/head0000.png")))

(def player @{:target @[0 0]
              :speed 2
              :in @[0 0]
              :walk-angle 0
              :legs-target-angle 0
              :pos @[50 50]
              :angles @{:head 0
                        :body 0
                        :legs 0}})


(def gos @[player])

(defn old-atan2
  [x y]
  (-> (math/atan2 y x)
      (/ math/pi)
      (* 180)))

(defn atan2
  [[x y]]
  (math/atan2 y x))

(defn angle-diff
  [a1 a2]
  (math/atan2 (math/sin (- a1 a2)) (math/cos (- a1 a2))))

(defn normalize-angle
  [a]
  (cond (<= a (- math/pi))
    (+ a (* 2 math/pi))

    (> a math/pi)
    (- a (* 2 math/pi))

    a))

(defn pos-angle
  [a]
  (if (neg? a)
    (+ a (* 2 math/pi))
    a))

(defn vector-angle
  ``
  Returns angle in radians between two vectors.
  Angle will always be -PI < a <= PI

  -PI/2 means v2 is 90 degrees to the left of v1
   PI/2 means v2 is 90 degrees to the right of v1
  Thus negative numbers = "v2 to the left"
       positive numbers = "v2 to the right"
  ``
  [v1 v2]
  (normalize-angle (- (atan2 v2) (atan2 v1))))

(defn pos-deg
  [v]
  (mod v 360))

(defn rad->deg
  [r]
  (* 180 (/ r math/pi)))

(defn v-zero?
  [[x y]]
  (and (zero? x)
       (zero? y)))

(defn clamp
  [v mi ma]
  (min ma (max mi v)))

(defn lerp
  [start stop t]
  (+ start (* t (- stop start))))

(defn render
  [el]
  (clear-background (map |(/ $ 255) [24 10 10]))

  (put player :target-angle (atan2 (v/v- (player :target) (player :pos))))
  (unless (v-zero? (player :in))
    (let [a (atan2 (player :in))]
      (put player :walk-angle a)))

  (put player :legs-target-angle (player :walk-angle))

  (def lookaway (-> (- (player :target-angle)
                       (player :walk-angle))
                    normalize-angle
                    (/ math/pi)))

  (when (> (math/abs lookaway) 0.45)
    (update player :legs-target-angle (fn [a] (normalize-angle (- a math/pi)))))

  (let [{:legs la
         :body ba} (in player :angles)
        leg-dif (angle-diff (player :legs-target-angle) la)
        forbidden-angle 0.5

        back (+ ba math/pi)
        limit (/ math/pi 4)
        right (pos-angle (- back limit))
        left (+ right limit limit)
        left (mod left (* 2 math/pi))
        right (mod right (* 2 math/pi))
        pain-angles [left right]

        ta (pos-angle (player :legs-target-angle))
        la (if (neg? la)
             (+ la (* 2 math/pi))
             la)
        la (if (> (- ta la) math/pi)
             (+ la (* 2 math/pi))
             la)
        ta (if (> (- la ta) math/pi)
             (+ ta (* 2 math/pi))
             ta)

        a (+ la (clamp (* 0.05
                          leg-dif) -0.05 0.05))

        [start stop] [(min ta la)
                      (max ta la)]
        [ostart ostop] [(min left right)
                        (max left right)]
        hit (or (and (>= start ostart)
                     (<= start ostop))
                (and (>= stop ostart)
                     (<= stop ostop))
                (and (>= ostart start)
                     (<= ostart stop))
                (and (>= ostop start)
                     (<= ostop stop)))]

    (when false # broke-limit
      (draw-text "OUCH" [10 30] :color :white))

    '(defer (rl-pop-matrix)
       (rl-push-matrix)
       (rl-scalef 0.33333 0.33333 1)
       (draw-text (string/format "pain angles: %0.2f %0.2f"
                                 ;pain-angles)
                  [10 10] :color :white
                  :font :monospace)
       (draw-text (string/format "target angle: %0.2f"
                                 ta)
                  [10 30] :color :white
                  :font :monospace)
       (draw-text (string/format "leg angle: %0.2f"
                                 la)
                  [10 50] :color :white
                  :font :monospace))

    (unless (v-zero? (player :in))
      (put-in player [:angles :legs] (normalize-angle a)))

    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-translatef ;(player :pos) 0)
      (draw-line 0 0 ;(->> (-> [(math/cos left) (math/sin left)]
                               (v/v* 100))
                           (map math/floor)) :blue)
      (draw-line 0 0 ;(->> (-> [(math/cos right) (math/sin right)]
                               (v/v* 100))
                           (map math/floor)) :purple)

      (let [ta (player :legs-target-angle)]
        (draw-line 0 0 ;(->> (-> [(math/cos ta) (math/sin ta)]
                                 (v/v* 100))
                             (map math/floor)) (if hit
                                                 :red
                                                 :green)))

      (draw-line 0 0 ;(->> (-> [(math/cos la) (math/sin la)]
                               (v/v* 100))
                           (map math/floor)) (if hit
                                               :red
                                               :green))

      (draw-line 0 0 ;(->> (-> [(math/cos (lerp la ta 0.5))
                                (math/sin (lerp la ta 0.5))]
                               (v/v* 100))
                           (map math/floor)) :white))

    (update player :pos v/v+
            (-> (v/normalize (player :in))
                (v/v* (player :speed))
                (v/v* (max 0.5 (- 1 (math/abs (/ leg-dif math/pi)))))
                (v/v* (max 0.5 (- 1 (math/abs lookaway)))))))

  (update-in player [:angles :head]
             (fn [a]
               (-> (+ a (* 0.5 (angle-diff (player :target-angle) a)))
                   normalize-angle)))

  (update-in player [:angles :body]
             (fn [a]
               (-> (+ a (* 0.1 (angle-diff
                                 (+ (/ (- (* 12 #(math/random)
                                             1) 6) 180)
                                    (player :target-angle))
                                 a)))
                   normalize-angle)))

  '(let [{:body ba
          :head ha
          :legs la} (player :angles)
         body-diff (- ba la)
         h-diff 1.5
         diff 1.5]
     (draw-text body-diff [10 10] :color :white)

     (when (and (v-zero? (player :in))
                (> (+ -0.01 (math/abs body-diff)) (math/abs diff)))
       (update-in player [:angles :legs]
                  (fn [a]
                    (+ a (* 0.03 body-diff)))))

     (update-in player [:angles :head]
                (fn [a]
                  (clamp a (- la h-diff) (+ la h-diff))))

     (update-in player [:angles :body]
                (fn [a]
                  (clamp a (- la diff) (+ la diff)))))

  #(update-in player [:angles :legs] (fn [a] (+ a (* 0.1 (- (player :target-angle) a)))))
  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef ;(player :pos) 0)

    (def angles (player :angles))

    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (rad->deg (angles :legs)) 0 0 1)
      (draw-texture legs -5 -15 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (rad->deg (angles :body)) 0 0 1)
      (draw-texture body -5 -13 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (rad->deg (angles :head)) 0 0 1)
      (draw-texture head -5 -9 :white))
    #(draw-circle 0 0 1 :white)
))

(def down-key
  @{:a [[:in 0] dec]
    :d [[:in 0] inc]
    :w [[:in 1] dec]
    :s [[:in 1] inc]})

(def up-key
  @{:a [[:in 0] inc]
    :d [[:in 0] dec]
    :w [[:in 1] inc]
    :s [[:in 1] dec]})

(defn on-event
  [el ev]
  (match ev
    {:mouse/move p}
    (put player :target p)

    ({:key/down k} (down-key k))
    (update-in player ;(down-key k))

    ({:key/release k} (up-key k))
    (update-in player ;(up-key k))))

(start-game {:render render
             :on-event on-event
             :init init
             :scale 3})
