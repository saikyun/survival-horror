(import ./state :as s)

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
    (put s/player :target p)

    ({:key/down k} (down-key k))
    (update-in s/player ;(down-key k))

    ({:key/release k} (up-key k))
    (update-in s/player ;(up-key k))))
