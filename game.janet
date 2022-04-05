(use freja/flow)

(defonce legs (load-texture "assets/legs0000.png"))
(defonce body (load-texture "assets/body0000.png"))
(defonce head (load-texture "assets/head0000.png"))

(def player @{:target @[0 0]
              :in @[0 0]
              :pos @[50 50]
              :angles @{:head 90
                        :body 80
                        :legs 70}})

(def gos @[player])

(defn atan2
  [x y]
  (-> (math/atan2 y x)
      (/ math/pi)
      (* 180)))

(defn pos-deg
  [v]
  (mod v 360))

(defn render
  [el]
  (clear-background (map |(/ $ 255) [24 10 10]))
  (put player :target-angle
       (pos-deg (+ -90 (atan2 ;(v/v- (player :pos) (player :target))))))
  (put player :walk-angle (pos-deg (+ 90 (atan2 ;(player :in)))))

  (def lookaway
    (/
      (math/abs
        (- 180 (pos-deg (- (player :target-angle)
                           (player :walk-angle)))))
      180))

  (update player :pos v/v+ (-> (v/normalize (player :in))
                               (v/v* lookaway)))

  (update-in player [:angles :head] (fn [a] (+ a (* 0.5 (- (player :target-angle) a)))))
  (update-in player [:angles :body] (fn [a] (+ a (* 0.1 (- (+ (- (* 12 (math/random)) 6)
                                                              (player :target-angle)) a)))))
  (update-in player [:angles :legs]
             (fn [a] (+ a (* 0.1 (- (player :walk-angle) a)))))

  #(update-in player [:angles :legs] (fn [a] (+ a (* 0.1 (- (player :target-angle) a)))))
  (defer (rl-pop-matrix)
    (rl-push-matrix)
    (rl-translatef ;(player :pos) 0)

    (def angles (player :angles))

    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (angles :legs) 0 0 1)
      (draw-texture legs -15 -10 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (angles :body) 0 0 1)
      (draw-texture body -15 -35 :white))
    (defer (rl-pop-matrix)
      (rl-push-matrix)
      (rl-rotatef (angles :head) 0 0 1)
      (draw-texture head -10 -10 :white))
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
             :scale 3})
