(ns justino.debug)

(defmacro dbg [s] `(let [x# ~s] (println '~s " -> " x#) x#))
