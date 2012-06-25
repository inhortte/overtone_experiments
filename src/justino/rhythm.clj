(ns justino.rhythm)

(def LENGTHS {:whole 4 :half 2 :quarter 1 :eighth 1/2 :sixteenth 1/4
              :whole-dot 6 :half-dot 3 :quarter-dot 3/2 :eighth-dot 3/4
              :sixteenth-dot 3/8 :eighth-trip 1/3 :quarter-trip 2/3
              :half-trip 4/3 :whole-trip 8/3})

(defn lengths->beats [v]
  (reduce #(conj %1 (+ (last %1) (LENGTHS %2))) [0] v))

