(import ./state :as s)
(use freja/flow)
(import freja/vector-math :as v)

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
  (when-let [p (ev :mouse/pos)]
    (let [delta (v/v- p s/last-mouse-pos)]
      (put s/player :mouse/delta delta)
      (update s/player :target v/v+ delta)
      (set s/last-mouse-pos p))

    (when (el :focused?)
      (disable-cursor)))

  (match ev
    {:mouse/down _}
    (put s/player :grabbing true)

    {:mouse/release _}
    (put s/player :grabbing false)

    ({:key/down k} (down-key k))
    (update-in s/player ;(down-key k))

    ({:key/release k} (up-key k))
    (update-in s/player ;(up-key k))))
