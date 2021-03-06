(ns qgame.utils.general
  "Some general utilities that have nothing in particular to do with quantum computing, matrix math, or complex numbers.")

(defn update-sub
  "Updates a sub-structure of m. Gets a list of values, each corresponding to a key in ks, calls f on that list (does *not* apply f), and assoc-s the elements of the resultant list back in, each resultant element corresponding, by order, to a key in ks. For function f, the number of inputs is expected to equal the number of outputs. Furthermore, the nth output should correspond to an 'updated' nth input."
  [m f ks]
  (->> (map (partial get m) ks)
       f
       (zipmap ks)
       (reduce-kv assoc m)))

(defn itermap
  "Iteratively concats init and f mapped down init."
  [f init coll]
  (if-let [s (seq coll)]
    (itermap f
             (concat init
                     (map #(f % (first s))
                          init))
             (rest s))
    init))

(defn bit-size
  "Given some positive integer x, gives the number of bits needed to represent it."
  [x]
  (loop [n 1]
    (if (<= x (bit-shift-left 1 n))
      n
      (recur (inc n)))))

(defn anywhere? ;Inspiration creds to DaoWen's find-in
  "Returns the truthy value of pred upon first encountering it in x, going deeper into x if still unfound."
  [pred x]
  (or (pred x)
      (and (coll? x)
           (some (partial anywhere? pred) x))))

;(defn regex-join
;  [& re-or-strs]
;  (->> re-or-strs
;       (map #(if-let [src (.-source %)] src %))
;       (apply str)
;       re-pattern))

(defn regex-join
  [& re-or-strs]
  (->> re-or-strs
       ;(map #(if-let [src (.-source %)] src %))
       (apply str)
       re-pattern))

(defn errant?
  "Predicate for testing whether an expression has any errors, even nested errors."
  [expression]
  (anywhere? (every-pred map? :error)
             expression))
