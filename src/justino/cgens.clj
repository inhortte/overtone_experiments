(ns justino.cgens
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

