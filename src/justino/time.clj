(ns justino.time
  (:use [overtone.core]))

(defn fun-wrap [f & args]
  (fn [] (apply f args)))

(defn beat-len [m] (- (metro 2) (metro 1)))
(defn til-next-beat [m] (- (m (m)) (now)))
(defn start-next-beat [m beats f]
  (after-delay (til-next-beat m)
               (let [pf (fun-wrap (periodic (* beats (beat-len m))
                                            f))]
                 pf)))
(defn ping [m amp freq inst]
  (after-delay 0 (fn [] (inst :amp amp :freq freq :pan -1)))
  (after-delay (beat-len m) (fn [] (inst :amp amp :freq freq :pan 1))))
