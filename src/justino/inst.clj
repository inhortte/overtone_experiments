(ns justino.inst
  (:use [overtone.studio inst]
        [overtone.core]))

(definst tblisi
  [note       {:default 440 :min 0 :max 120 :step 1}
   wave       {:default 1 :min 0 :max 2 :step 1}
   r          {:default 0.8 :min 0.01 :max 0.99 :step 0.01}
   attack     {:default 0.01 :min 0.001 :max 4 :step 0.001}
   decay      {:default 0.1 :min 0.001 :max 4 :step 0.001}
   sustain    {:default 0.6 :min 0.001 :max 0.99 :step 0.001}
   release    {:default 0.01 :min 0.001 :max 4 :step 0.001}
   cutoff     {:default 100 :min 1 :max 20000 :step 1}
   env-amount {:default 0.01 :min 0.001 :max 4 :step 0.001}]
  (let [freq       note
        freqs      [freq (* 1.01 freq) (* 0.99 freq)]
        vol-env    (env-gen (adsr attack decay sustain release)
                            (line:kr 1 0 (+ attack decay release))
                            :action FREE)
        fil-env    (env-gen (perc))
        fil-cutoff (+ cutoff (* env-amount fil-env))
        waves     (* vol-env
                     [(saw freqs)
                      (pulse freqs 0.5)
                      (lf-tri freqs)])
        selector   (select wave waves)
        filt       (rlpf selector fil-cutoff r)]
    (* 0.5 filt)))

(definst fma [freq-a 440 amp-a 0.7 freq-b 150 amp-b 0.7]
  (let [osc-a (* amp-a (sin-osc freq-a))
        osc-b (sin-osc (+ freq-b osc-a))]
    osc-b))

(definst fm-demo [freq 330 amp 0.2 amp-b 0.2]
  (let [osc-a (* (sin-osc (mouse-x 20 3000))
                 0.2)
        osc-b (* amp (sin-osc (* (mouse-y 3000 0) osc-a)))]
    osc-a))

(definst theramin-alpha [freq-low 220 freq-high 880 amp 0.1]
  (let [osc (pan2:ar (sin-osc:ar (mouse-x:kr freq-low freq-high)
                                 0 amp))]
    osc))

(definst xenakis [n 11]
  (let [freq (+ (rand 510) 50)
         numcps (+ (rand-int 18) 2)
         osc (resonz:ar
              (mix (pan2:ar
                    (gendy1:ar (rand-int 7)
                               (rand-int 7)
                               (rand) (rand)
                               freq freq
                               (rand) (rand)
                               numcps
                               (sin-osc:kr
                                (+ (rand 0.18) 0.02) 0))
                    (- (rand 2) 1)))
              (mouse-x:kr 100 2000)
              (mouse-y:kr 0.01 1))]
    osc))
