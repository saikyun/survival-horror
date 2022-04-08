(import freja/flow)

# right is 0
# counter clockwise is -
# clockwise is +
# 0.5 is thus down

(defn ->deg
  [t]
  (math/round (* 180 t)))

(defn ->rad
  [t]
  (* t math/pi))

(defn rad->tau
  [r]
  (/ r math/pi))

(defn normalize
  [t]
  (mod t 2))

(def sin (comp math/sin ->rad))
(def cos (comp math/cos ->rad))
(def acos (comp rad->tau math/acos))

(defn atan2
  ``
  atan2, returns angle in tau.
  Takes x then y, so one can splice x/y vectors.
  ``
  [x y]
  (-> (math/atan2 y x)
      rad->tau))

(defn shortest-angle
  ``
  Returns shortest angle between angles a1 and a2.
  Is negative if a2 is counter-clockwise of a1.
  ``
  [a1 a2]
  (let [v (- a2 a1)]
    (cond
      (> v 1)
      (- v 2)

      (< v -1)
      (+ v 2)

      v)))

### sector

(defn sector-overlap
  ``
  Takes two sectors with angles start and stop.
  Returns true if they overlap, otherwise false.
  ``
  [[start1 stop1] [start2 stop2]]

  (comptime
    (defn radius [start stop]
      (* 0.5 (math/abs (- stop start)))))

  (comptime
    (defn middle-angle [start stop radius]
      (+ (normalize (min start stop)) radius)))

  (let [r1 (radius start1 stop1)
        a1 (middle-angle start1 stop1 r1)
        r2 (radius start2 stop2)
        a2 (middle-angle start2 stop2 r2)
        ad (math/abs (shortest-angle a1 a2))]
    (<= ad (+ r1 r2))))


(defn angle-clamp
  [start stop a]
  (let [start start
        stop stop
        rad (* 0.5 (math/abs (- start stop)))
        middle-angle (+ start rad)
        dist (shortest-angle a middle-angle)]
    (cond
      (< rad dist)
      start

      (> (- rad) dist)
      stop

      a)))

# render sector

(defn draw-circle-sector
  [pos rad s1 s2 segments color]
  (let [sector
        ;(map |(-> $
                   ->deg
                   (- 90)
                   (* -1))
              [s1 s2])]
    (flow/draw-circle-sector
      pos
      rad
      ;sector
      segments
      color)))

### vector

(defn vector-shortest-angle
  ``
  Returns angle in radians between two vectors.
  Angle will always be -PI < a <= PI

  -PI/2 means v2 is 90 degrees to the left of v1
   PI/2 means v2 is 90 degrees to the right of v1
  Thus negative numbers = "v2 to the left"
       positive numbers = "v2 to the right"
  ``
  [x1 y1 x2 y2]
  (shortest-angle (atan2 x1 y1) (atan2 x2 y2)))
