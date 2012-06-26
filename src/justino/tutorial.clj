(ns justino.tutorial
  (:use [overtone.core]))

(definst wn-lpf [amp 0.1 freq 1000]
  (lpf:ar (white-noise:ar amp) freq))

;; (ctl wn-lpf :freq 5000)

(definst wn-line [amp 0.1 freq-h 5000 freq-l 500 duration 10]
  (lpf:ar (white-noise:ar amp) (line:kr freq-h freq-l duration)))

;; resonz filter

(definst lfnoise-resonz [freq 400 start 10000 end 1000 duration 10]
  (resonz:ar (lf-noise0:ar 400) (line:kr start end duration 0.1)))

(definst saw-resonz [freq 220 start 10000 end 1000 duration 10]
  (resonz:ar (saw freq) (line:kr start end duration 0.1)))

(definst saw-resonz [freq  {:default 220}
                     start {:default 4000}
                     end   {:default 100}
                     dur   {:default 10}]
  (let [osc (mix (saw (* freq [0.99 1 1.01])))
        lin (line:kr start end dur)
        resonator (resonz osc lin 0.1)]
    resonator))

(definst panning-resonz [freq  {:default 220}
                     start {:default 4000}
                     end   {:default 100}
                     dur   {:default 10}]
  (let [osc (mix (saw (* freq [0.99 1 1.01])))
        lin (line:kr start end dur)
        resonator (resonz osc lin 0.1)]
    (pan2:ar resonator (mouse-x:kr -1 1))))

(definst sin-pan [amp 0.4]
  (pan2 (mix (* amp (sin-osc:ar [220 660]))) (mouse-x -1 1)))

;; Sawtooth wave: Add up n harmonics with amplitude falling off
;; as 1/harmonicnumber, sign alternates between +1 and -1
(definst saw-me [freq 220 amp 0.4]
  (let [oscs (mix
              (map #(* (sin-osc (* freq (inc %))) (Math/pow -1 (inc %))
                       (/ 0.5 (inc %)) amp)
                   (range 10)))]
    (pan2 oscs 0.0)))

;; Square wave: Sum of odd harmonics, no even, amplitude falls
;; as off 1/harmonicnumber; closest 'real' waveform is a clarinet tone
(definst square-me [freq 220 amp 0.4]
  (let [oscs (mix
              (map (fn [i]
                     (let [harmonic-number (inc (* 2 i))]
                       (* (sin-osc (* freq harmonic-number))
                          (/ 1.0 harmonic-number)
                          amp)))
                   (range 10)))]
    (pan2 oscs 0.0)))

;; Triangle wave: also odd harmonics only, falls off as
;; 1 over harmonicnumber squared with alternating sign
(definst triangle-me [freq 220 amp 0.4]
  (let [oscs (mix
              (map (fn [i]
                     (let [harmonic-number (inc (* 2 i))]
                       (* (sin-osc (* freq harmonic-number))
                          (Math/pow -1 i)
                          (/ 1.0 (* harmonic-number harmonic-number))
                          amp)))
                   (range 10)))]
    (pan2 oscs 0.0)))

(definst multiple-sines [freq 220 amp 0.4]
  (let [osc (mix
             (map (fn [i]
                    (* (sin-osc (* freq (inc i)))
                       (/ 1.0 (inc i))
                       amp))
                  (range 10)))]
    (pan2 osc 0.0)))

(definst wobbled [pitch 220 amp 0.4 wobble-freq 3 wobble-depth 5]
  (let [wobbler (* wobble-depth (sin-osc wobble-freq))
        freq (+ pitch wobbler)]
    (* amp (saw :freq freq))))

(definst wobbled [pitch 220 amp 0.4 wobble-freq 2 wobble-index 4/2]
  (let [wobbler (* wobble-index wobble-freq (sin-osc wobble-freq))
        freq (+ pitch wobbler)]
    (* amp (square freq))))

;; dangerous!
(definst harmonicity [pitch 220 amp 0.4]
  (let [harm (round (mouse-x 1 10) 1)
        modindex (mouse-y 0.0 10.0)
        modfreq (* pitch (/ 1.0 harm))
        wobbler (* modindex modfreq (sin-osc modfreq))]
    (* 0.01 (+ (square pitch) wobbler))))

;; so why doesn't this work?
;; because everything in the arguments list is a ControlProxy! Bastards!
(definst two-note-wobble [freq-1 220 freq-2 440 wobble-freq 2 wobble-depth 4]
  (let [wobble-freq-2 (* (:value wobble-freq)
                         (/ (float (:value freq-2)) (:value freq-1)))
        wobbler-1 (* wobble-depth (sin-osc wobble-freq))
        wobbler-2 (* wobble-depth (sin-osc wobble-freq-2))
        freqs [(+ freq-1 wobbler-1) (+ freq-2 wobbler-2)]]
    (mix (map sin-osc freqs))))

;; straightforward modulated sine
(definst mod-sine []
  (let [carrfreq (mouse-x 110 880 LIN)
        modfreq (mouse-y 0.1 10 LIN)
        modulator (* 4 (sin-osc modfreq))
        carrier (* 0.4 (sin-osc (+ modulator carrfreq)))]
    carrier))

;; pulse mod
(definst pulse-mod [freq 220 amp 0.4 rate 1 release 1]
  (let [mod (+ 0.5 (* 0.4 (sin-osc rate)))
        env (env-gen:kr (asr 0.1 1 release)
                        (line:kr 1 0 (+ 0.1 release)) :action FREE)]
    (* amp env (pulse freq mod))))

;; ring-modulation
(definst ring-square []
  (let [c-freq (mouse-x 55 880 EXP)
        m-freq (mouse-y 20 1000 EXP)
        carrier (* 0.4 (square c-freq))
        modulator (* 0.4 (square m-freq))]
    (* carrier modulator)))

;; amplitude modulation
(definst amp-mod []
  (let [c-freq (mouse-x 55 880 EXP)
        m-freq (mouse-y 1 1000 EXP)
        carrier (* 0.4 (square c-freq))
        modulator (* 0.4 (+ 0.25 (* 0.25 (sin-osc m-freq))))]
    (* carrier modulator)))

;; phase modulation
(definst phase-mod [freq 220]
  (let [p-freq (mouse-x 0 100 LIN)
        p-mod (+ Math/PI (* Math/PI (sin-osc p-freq)))
        am-freq (mouse-y 1 250 EXP)
        am-mod (* 0.4 (+ 0.25 (* 0.25 (sin-osc am-freq))))]
    (* am-mod (sin-osc freq p-mod))))

(definst phase-mod [freq 220]
  (let [mod-freq (mouse-x 0 1000)
        pm-index (mouse-y 0.0 10.0)]
    (pm-osc freq mod-freq pm-index)))

;; try sin-osc-fb

;; chorus with line & x-line - start of my resonances!
(definst tlusty-chorus [freq 220 rf 10000 attack 0.2 release 2 gate 1]
  (let [ch-freqs (* freq [0.99 1 1.01])
        res-freq (x-line rf 10 release)
        bwr (line 1 0.05 release)
        env (env-gen (asr attack 1 release) gate :action FREE)]
    (resonz (* env (saw ch-freqs)) res-freq bwr)))

(comment
  (do (tlusty-chorus 220 5000 18)
      (tlusty-chorus 110 10000 20)
      (tlusty-chorus 55 15000 22)))

(definst tlusty-chorus-w-am [freq 220 rf 10000 decay 2]
  (let [ch-freqs (* freq [0.99 1 1.01])
        res-freq (x-line rf 10 decay)
        bwr (line 1 0.05 decay)
        am (+ 0.5 (* 0.5) (lf-saw (line 1 20 decay)))]
    (* am (resonz (saw ch-freqs) res-freq bwr))))

(def s7 (load-sample "resources/public/sounds/snare07.ogg"))
(def cello (load-sample "resources/public/sounds/cello01.ogg"))

(definst cello-vibrato [harmonic 1 v-depth 0.02 v-speed 8]
  (let [buf-id (:id cello)
        modulator (+ 1 (* v-depth (sin-osc v-speed)))]
    (play-buf 1 buf-id (* (buf-rate-scale buf-id) harmonic modulator) 1 0 1)))

;; bell
(def bell-specs [0.5,1,1.19,1.56,2,2.51,2.66,3.01,4.1])
(def bell-amps [0.25,1,0.8,0.5,0.9,0.4,0.3,0.6,0.1])
(definst bell [freq 440 amp 0.4]
  (let [osc (mix
             (->> (map #(* freq %) bell-specs)
                  (map sin-osc)
                  (map * bell-amps)
                  (map #(* amp %))))]
    (* (line:kr amp 0.0 10) osc)))

;; improved bell!
(definst bell2 [freq 440 amp 0.4]
  (let [num-partials (count bell-specs)
        vibrato-freqs (map (fn [_] (rand 5)) (range num-partials)) 
        tremolo-freqs (map (fn [_] (+ 0.1 (rand 2.9))) (range num-partials))
        decay-times (map (fn [i]
                           (+ 2.5
                              (rand (* 5 (- 1.0 (/ (float i)
                                                   num-partials))))))
                         (range num-partials))
        freqs (map #(* freq (+ %1 (* 0.005 (sin-osc:kr %2))))
                   bell-specs vibrato-freqs)
        amps (map #(* 0.1
                      (* (+ 0.9 (* 0.1 (sin-osc %2))) %1)
                      (line 1 0 %3))
                  bell-amps tremolo-freqs decay-times)]
    (mix (map #(pan2 (* %2 (sin-osc %1)) (dec (rand-int 3)))
              freqs amps))))

;; envelopes

;; chirping!
(definst chirp [top 10000 bottom 20 dur 0.5 max-level 0.3]
  (let [freq-env (env-gen:kr (envelope [top bottom] [dur]) FREE)
        amp-env (env-gen:kr (envelope [max-level 0] [dur]) FREE)]
    (* amp-env (saw freq-env))))

(definst springy-buttock [pitch 220 depth 10 rate 10 dur 1 level 0.3]
  (let [freq (+ pitch (* depth (sin-osc rate)))
        env (env-gen:kr (envelope [level 0] [dur]) FREE)]
    (* env (saw freq))))

;; let's try some impulse - not working.
(defcgen tinkle
  [freq {:default 220 :doc "initial frequency"}
   harms {:default 3
          :doc "number of harmonics - all harmonics over 4 will be lowered to the source octave"}]
  (:ar
   (letfn [(rts [s n]
             (let [r [(- s (/ s 2.0)) (+ s (/ s 2.0))]]
               (loop [n n]
                 (if (and (>= n (first r)) (<= n (second r))) n
                     (recur (/ n 2.0))))))]
     (let [notes (map #(rts freq (* freq (inc %))) (range harms))
           env (decay (t2a (demand (impulse:kr 40) 0
                                   (dseq [0 1] INF))) 0.7)
           bing (* 0.5 (* 7 env) (sin-osc (nth notes (rand-int (count notes)))))]
       (clip2 bing 1)))))

;; sequencing

(defn pud-pan [m]
  (after-delay 0 (fn [] (pud :pan -1)))
  (after-delay (beat-len m) (fn [] (pud :pan 1))))

