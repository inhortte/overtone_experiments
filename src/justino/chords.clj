(ns justino.chords
  (:use [overtone.core]))

(defcgen floating-cgen [pitch {:default 220} amp {:default 0.4}]
  (:ar
   (let [pul-am-env (env-gen (envelope [0.1 0.2 0.5 0.3]
                                       [(inc (rand-int 5))
                                        (inc (rand-int 10))
                                        (inc (rand-int 10))]
                                       :linear 2 0) FREE)
         pul-width-mod (+ 0.3 (* 0.2 (sin-osc (/ 1.0 (inc (rand-int 16))))))
         pul-res-env (env-gen (envelope [200 200 500 4000 1000]
                                        [1 (inc (rand-int 10))
                                         (inc (rand-int 20))
                                         (inc (rand-int 15))]
                                        :exponential 3 0) FREE)
         pul-osc (* pul-am-env (pulse pitch pul-width-mod))
         pul-res (resonz pul-osc pul-res-env)]
     (* amp pul-res))))

(defsynth floating-chord-synth [amp 0.4 n1 65.40639132514966 n2 77.78174593052022 n3 97.99885899543733]
  (let [pitches [n1 n2 n3]
        amp-each (/ (:value amp) (count pitches))
        floatings (floating-cgen pitches)]
    (out 0 (* amp (splay floatings :spread 1 :center 0)))))

(defn floating-chord [amp & ch]
  "Returns a function which takes the same inputs for modification."
  (let [[n1 n2 n3] (map midi->hz (apply chord ch))
        s (floating-chord-synth amp n1 n2 n3)]
    (println (:id s))
    (fn [new-amp & ch]
      (let [pitches (map midi->hz (apply chord ch))
            new-amp (or new-amp amp)]
        (when new-amp (ctl (:id s) :amp new-amp))
        (doseq [note (seq (zipmap [:n1 :n2 :n3] pitches))]
          (ctl (:id s) (first note) (second note)))))))



