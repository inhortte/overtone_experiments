(ns justino.atonal
  (:use [overtone.core]))

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
