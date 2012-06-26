(ns justino.synths
  (:use [overtone.core]
        justino.cgens))

(defsynth floating-chord-synth [amp 0.4 n1 65.40639132514966 n2 77.78174593052022 n3 97.99885899543733]
  (let [pitches [n1 n2 n3]
        amp-each (/ (:value amp) (count pitches))
        floatings (floating-cgen pitches)]
    (out 0 (* amp (splay floatings :spread 1 :center 0)))))

(defsynth pud [amp 0.5 freq 220 pan 0.0]
  (let [env (env-gen (envelope [0 1 1 0] [0.1 0.1 0.5]) FREE)
        osc (pan2 (* (/  amp 2.0) env
                     (+ (saw (* 0.5 (/ freq 2.0))) (blip freq))) pan)]
    (out 0 osc)))

(defsynth droplet-synth [amp {:default 0.4} pitch {:default 220}
                         v-rate {:default 4} v-depth {:default 4}
                         thick {:default 0.2 :min 0.0 :max 1.0}]
  (let [low-env (env-gen (envelope [0.01 thick 0.01 0.0] [0.5 0.5 0.5])
                         :action FREE)
        mid-env (env-gen (envelope [0.01 thick 0.01 0.0] [0.5 0.5 0.5])
                         :action FREE)
        high-env (env-gen (envelope [0.01 0.8 0.01] [0.01 1.5])
                          :action FREE)
        high-vibrato (* v-depth (sin-osc v-rate))
        low-osc (* low-env (pan2 (pulse (/ pitch 2.0) 0.1) -1))
        mid-osc (* mid-env (pan2 (saw (/ pitch 2.0)) 1))
        high-osc (* high-env (sin-osc (+ high-vibrato (* 2 pitch))))]
    (out 0 (* 0.3 amp (+ low-osc mid-osc high-osc)))))
