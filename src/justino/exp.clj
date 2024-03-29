(ns justino.exp
  (:use [overtone.live]))

(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq (midicps note)
        noize (* 0.8 (white-noise))
        dly (/ 1.0 freq)
        plk (pluck noize gate dly dly decay coef)
        dist (distort plk)
        filt (rlpf dist (* 12 freq) 0.6)
        clp (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.01 dur) :action FREE) reverb)))

(def degrees [:vi :vii :i+ :_ :vii :_ :i+ :vii :vi :vii :_])

(def pitches (degrees->pitches degrees :diatonic :c3))

(defn play [time notes step]
  (let [note (first notes)]
    (when note
      (at time (plucked-string note)))
    (let [next-time (+ time step)]
      (apply-at next-time play [next-time (rest notes) step]))))

