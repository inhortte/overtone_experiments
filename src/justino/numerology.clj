(ns justino.numerology
  (:use [justino.debug]))

(def pi (slurp "resources/public/pidigits.txt"))
(def phi (slurp "resources/public/phidigits.txt"))

(defn string-to-bigint [s]
  (->> (seq s)
       (map #(Character/getNumericValue %))
       (reduce #(+ (* 10M %1) %2) 0)))

(defn part-str-to-bigints [n s]
  (->> (seq s)
       (partition n)
       (map #(string-to-bigint ((partial apply str) %)))))

(defn- per-digit-mod [num-str length modulus]
  (->> (seq (subs num-str 0 length))
       (map #(mod (Character/getNumericValue %) modulus))))

(defmacro pdm [num]
  `(def ~(symbol (str num "-per-digit-mod"))
       (fn [length# modulus#]
         (per-digit-mod ~(symbol num) length# modulus#))))

(pdm "pi")
(pdm "phi")

(defn- chunk-mod [num-str length chunk modulus]
  (map #(mod % modulus)
       (part-str-to-bigints chunk (subs num-str 0 length))))

(defmacro cm [num]
  `(def ~(symbol (str num "-chunk-mod"))
       (fn [length# chunk# modulus#]
         (chunk-mod ~(symbol num) length# chunk# modulus#))))

(cm "pi")
(cm "phi")

(defn gmulpk [filename chunk pi-digits modulus]
  (binding [*out* (java.io.FileWriter. filename)]
    (doseq [part (part-str-to-bigints chunk (subs pi 0 pi-digits))]
      (prn [part (mod part modulus)]))))
