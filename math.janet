
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

(defn v-zero?
  [[x y]]
  (and (zero? x)
       (zero? y)))
