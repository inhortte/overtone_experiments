(ns justino.numerology)

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

(defn pi-per-digit-mod [length modulus]
  (per-digit-mod pi length modulus))

(defn phi-per-digit-mod [length modulus]
  (per-digit-mod phi length modulus))

(defn- chunk-mod [num-str length chunk modulus]
  (map #(mod % modulus)
       (part-str-to-bigints chunk (subs num-str 0 length))))

(defn pi-chunk-mod [length chunk modulus]
  (chunk-mod pi length chunk modulus))

(defn phi-chunk-mod [length chunk modulus]
  (chunk-mod pi length chunk modulus))

(defn gmulpk [filename chunk pi-digits modulus]
  (binding [*out* (java.io.FileWriter. filename)]
    (doseq [part (part-str-to-bigints chunk (subs pi 0 pi-digits))]
      (prn [part (mod part modulus)]))))



