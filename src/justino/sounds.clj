(ns justino.sounds
  (:use [overtone.core]
        [overtone.inst [piano synth]]
        [justino.numerology :only [fibs-mod pi-chunk-mod]]))

(definst basic-sine [freq 440] (sin-osc freq))
(definst basic-saw [freq 220] (saw freq))
(definst basic-square [freq 220] (square freq))

(definst multiple-saw [freq 220]
  (+ (saw freq)
     (saw (* 2 freq))
     (saw (* 3 freq))))

(definst detuned-saw [freq 110]
  (mix (saw (* freq [0.99 1 1.01]))))

;;; A sin wave modifies a square wave! Imagine that?
(definst wobbled-sine [pitch-freq 220 wobble-freq 5 wobble-depth 5]
  (let [wobbler (* wobble-depth (sin-osc wobble-freq))
        freq (+ pitch-freq wobbler)]
    (sin-osc freq)))

(defn thurk-wobbled-sin []
  (wobbled-sine)
  (doseq [i (range 20)] (at (+ (now) (* i 2000)) (ctl wobbled-sine :wobble-depth i))))

(definst dubstep [freq 110 wobble-freq 1]
  (let [sweep (lin-exp (lf-saw wobble-freq) -1 1 40 2000)
        son (mix (saw (* freq [0.99 1 1.01])))]
    (lpf son sweep)))

(defn notecps [n]
  (midi->hz (note n)))

(defn prog-thurk-dubstep []
  (let [some-notes [:g2 :f2 :a2 :e2]
        series (zipmap (range 30) (take 30 (cycle some-notes)))]
    (dubstep)
    (doseq [n series]
      (at (+ (now) (* (key n) 2000)) (ctl dubstep :freq (notecps (val n)))))))

(def fib-degrees [:i :i# :ii :iii :iv :v :vi :vii])
(def fib-pitches (vec (degrees->pitches fib-degrees :hungarian-minor :c4)))
(def fib-series (map #(fib-pitches (int %)) (fibs-mod 120 8)))
(def fib-bass-notes (map #(- % 24) (take 8 fib-series)))

;;; why doesn't this fucking work?
(defn fibs-play [time notes sep]
  (let [n (first notes)]
    (when n
      (at time (piano n))
      (let [next-time (+ time sep)]
        (apply-at next-time fibs-play next-time (rest n) sep)))))

;;; my first success!
(defn fib-piano []
  (doseq [n (map #(vector %1 %2) (range 120) fib-series)]
    (at (+ (now) (* (n 0) 500)) (piano (n 1) 0.5 70))))

(defn fib-bass []
  (doseq [n (map #(vector %1 %2) (range 480)
                 (take 480 (cycle fib-bass-notes)))]
    (at (+ (now) (* (n 0) 125)) (ks1-demo (n 1) 1))))

(defn play-fib []
  (at (+ 1000 (now))
      (do
        (fib-piano)
        (fib-bass))))
