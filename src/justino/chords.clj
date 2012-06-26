(ns justino.chords
  (:use [overtone.core]
        justino.time
        justino.synths))

(defmacro chord-macro [name inst]
  `(def ~(symbol name)
     (fn [amp# & ch#]
       (let [pitches# (map midi->hz (apply chord ch#))
             s# (apply ~inst (cons amp# pitches#))]
         (fn [new-amp# & new-ch#]
           (let [new-pitches# (map midi->hz (apply chord new-ch#))
                 note-keys# (map #(keyword (str "n" (inc %)))
                                 (range (count new-pitches#)))]
             (when new-amp# (ctl (:id s#) :amp new-amp#))
             (doseq [note# (seq (zipmap note-keys# new-pitches#))]
               (ctl (:id s#) (first note#) (second note#)))))))))

(chord-macro "floating-chord" floating-chord-synth)

(defn with-cho-pro [m chopro beats inst amp]
  (after-delay
   (til-next-beat m)
   (let [pf (fun-wrap
             (let [cs (apply inst
                             (cons amp
                                   (rest (first chopro))))]
               (periodic (* beats (beat-len m))
                         (fn []
                          (apply cs (rand-nth chopro))))))]
     pf)))

(defn arpeg [m freqs beats inst amp & etc]
  (let [beats-len (* beats (beat-len m))
        beat-frac (/ beats-len (count freqs))
        freqs (shuffle freqs)]
    (periodic beats-len
              (fn []
                (->> (zipmap (range) freqs)
                     (seq) (sort)
                     (map #(after-delay (* beat-frac (first %))
                                        (fn []
                                          (apply inst (cons amp (cons (second %) etc))))))
                     (dorun))))))
