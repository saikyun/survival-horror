### math stuff that should be in a library

(defn between
  [start stop v]
  (and (>= v start)
       (< v stop)))

(defn clamp
  [v mi ma]
  (min ma (max mi v)))

(defn lerp
  [start stop t]
  (+ start (* t (- stop start))))

(defn v-lerp
  [[x1 y1] [x2 y2] t]
  [(lerp x1 x2 t) (lerp y1 y2 t)])

(defn v-zero?
  [[x y]]
  (and (zero? x)
       (zero? y)))
