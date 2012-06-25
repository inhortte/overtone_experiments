(ns justino.pinemarten
  (:use [overtone.core]
        [overtone.inst [piano drum synth]]
        [justino.rhythm]
        [justino.inst]))

(defn note->hz [n]
  (midi->hz (note n)))

(defn vec-or-note->hz
  "el can be a keyword or a vector of keywords. The return value is always a vector of hz."
  [el]
  (if (keyword? el) (vec-or-note->hz [el])
      (map note->hz el)))

(defmacro defplayer [name notes inst]
  `(def ~(symbol name)
     (fn [beat-num#]
       (let [rhythm# (butlast (lengths->beats (map #(second %) ~notes)))
             hz# (map vec-or-note->hz (map #(first %) ~notes))]
         (doseq [rhythm-hz# (map #(vector %1 %2) rhythm# hz#)]
           (at (metro (+ (first rhythm-hz#) beat-num#))
               (dorun (map ~inst (second rhythm-hz#)))))))))

(def metro (metronome 130))
(def bar-length 8)

(def smaller-one-notes [[:d4 :quarter-trip]
                        [:d4 :eighth-trip]
                        [:c4 :quarter]
                        [:d4 :quarter-trip]
                        [:d4 :eighth-trip]
                        [:c4 :quarter]
                        [:d4 :eighth-trip]
                        [:d4 :eighth-trip]
                        [:d4 :eighth-trip]
                        [:c4 :quarter-trip]
                        [:c4 :eighth-trip]
                        [:d4 :quarter]
                        [:d4 :quarter]])
(defn smaller-one-inst [& params]
  (apply tone-snare params))
(defplayer "smaller-one" smaller-one-notes smaller-one-inst)

(def paw-notes [[:a3  :quarter]
                [:g3  :quarter-trip]
                [:g3  :eighth-trip]
                [:a3  :quarter-trip]
                [:a3  :eighth-trip]
                [:g3  :quarter-trip]
                [:g3  :eighth-trip]
                [:a3  :quarter-trip]
                [:a3  :eighth-trip]
                [:g3  :quarter-trip]
                [:g3  :eighth-trip]
                [:a3  :quarter]
                [:bb3 :quarter-trip]
                [:g3  :eighth-trip]])
(defn paw-inst [& params]
  (apply tom params))
(defplayer "paw" paw-notes paw-inst)

(def elk-notes [[[:d5 :g5]  :eighth]
                [[:d5 :g5]  :eighth]
                [[:d5 :g5]  :quarter]
                [[:d5 :a5]  :quarter]
                [[:eb5 :g5] :eighth]
                [[:eb5 :g5] :eighth]
                [[:f5 :a5]  :quarter]
                [[:f5 :bb5] :quarter]
                [[:f5 :a5]  :quarter]
                [[:g5 :bb5] :quarter]])
(defn elk-inst [freq]
  (tblisi freq :wave 0 :attack 0.1 :sustain 0.5 :release 0.15 :cutoff 1300))
(defplayer "elk" elk-notes elk-inst)

(defn repeat-pattern [beat-num patt times]
  (dotimes [n times]
    (let [this-beat (+ beat-num (* n bar-length))]
      (at (metro this-beat) (patt this-beat)))))
