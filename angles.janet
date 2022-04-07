(defn tau->deg
  [t]
  (math/round (* 180 t)))

(defn rad->tau
  [r]
  (/ r math/pi))

(defn normalize
  [t]
  (mod t 2))

(defn between
  [start stop v]
  (and (>= v start)
       (< v stop)))

(defn angle-diff
  ``
  Returns shortest angle between angles a1 and a2.
  Is negative if a2 is counter-clockwise of a1.
  ``
  [a1 a2]
  (let [v (- a1 a2)]
    (cond
      (> v 1)
      (- v 2)

      (< v -1)
      (+ v 2)

      v)))

(defn overlap
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
        ad (math/abs (angle-diff a1 a2))]
    (<= ad (+ r1 r2))))
