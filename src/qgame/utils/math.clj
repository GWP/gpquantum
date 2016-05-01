(ns qgame.utils.math
  "Handles all of the math for qgame, including matrix math and complex number math. For all operations, converts the arguments so that any nested collectsion are math.js matrices, calls the operation, and then converts any nested matrices back to vectors."
  (:require 
            [clojure.walk :as w :refer [postwalk
                                        prewalk]]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.complex :as compl]
            [incanter.core :as ic]
            [complex.core :as c :refer [+ - / *]]))
;removed math.js and arndt.js from require

;Experimental math.js loader which won't require qgame library consumers to specify the math.js dependency foreign library file
;When trying this, remember to comment out the math.js require in the above namespace delcaration, and also to comment out the foreign libs usage
;(def mathscript (.createElement js/document "script"))
;(set! (.-type mathscript) "text/javascript")
;(set! (.-src mathscript) "http://cdnjs.cloudflare.com/ajax/libs/mathjs/0.18.0/math.min.js")
;(.appendChild (.-head js/document) mathscript)

;(def math (js/mathjs))

;Collection to matrix, and matrix to collection conversion
;(defn- to-matrix
;  "Internal function for converting a collection to a math.js matrix, leaving any non-collection unchanged."
;  [x]
;  (if (sequential? x)
 ;   (math.matrix. (to-array x))
 ;   x))

;(defn- nested-to-matrix
;  "Internal function for converting any/all nested collections to math.js matrices."
;  [coll]
;  (w/postwalk to-matrix coll))

;(defn- to-vec
;  "Internal function for converting a math.js matrix and/or a JavaScript array to a vector, leaving any non-math.js-matrix or non-array unchanged."
;  [x]
;  (cond
 ;   (array? x) (vec x)
 ;   (= (.typeof math x) "matrix") (vec (.toArray x))
 ;   :else x))

;(defn- nested-to-vec
;  "Internal function for converting any/all nested math.js matrices to vectors."
;  [mat]
;  (w/prewalk to-vec mat))

;(defn matrix-safe
;  "Calls a function on math.js-matrix-converted arguments then converts the result back to vectors."
;  ([f x]
;   (-> x nested-to-matrix f nested-to-vec))
;  ([f x y]
 ;  (let [x* (nested-to-matrix x)
 ;        y* (nested-to-matrix y)]
 ;    (nested-to-vec (f x* y*)))))

;Constants
(def pi Math/PI)

;(def sqrt2 (.-SQRT2 math))

(def sqrt1_2 (mat/sqrt 0.5))

;(def i (.-i math))
(def i (compl/complex 0 1))

(def e Math/E)

;Arithmetic
(defn abs ;included
  "Absolute value."
  [x]
  (mat/abs x))

;(defn round-1 ;included, sort of
;  "Rounds a number (or collection of numbers) to a certain number of digits."
 ; ([x]
 ;  (round x 0))
 ; ([x digits]
 ;  (matrix-safe #(math.round % digits) x)))

(defn round [x] (mat/round x)) ;how to round to variable number of digits?

(defn add ;included
  "Adds the given collections and numbers. Collections must have matching dimensions."
  ([x]
   (mat/add x 0))
  ([x y]
   (mat/add x y))
  ([x y & more]
   (reduce mat/add (mat/add x y) more)))

(defn subtract ;included
  "Subtracts the given collections and numbers. Collections must have matching dimensions."
  ([x]
   (mat/sub 0 x))
  ([x y]
   (mat/sub x y))
  ([x y & more]
   (reduce mat/sub (mat/sub x y) more)))

(defn multiply
  "Multiplies the given collections and numbers. Collections must have correct dimensions for matrix multiplication (i.e. for left collection size m x n, the right collection must be n x m)."
  [x y]
  (mat/inner-product x y))

(defn divide
  "Divides the given collections and numbers. Collections must have correct dimensions for matrix division (i.e. for left collection size m x n, the right collection must be n x m)."
  [x y]
  (mat/div x y))

(defn pow
  [x y]
  (mat/pow x y))

(defn sqrt
  "Square root."
  [x]
  (mat/sqrt x))

(defn cos
  "Cosine."
  [x]
  ;#_(-> x nested-to-matrix math.cos nested-to-vec)
  (mat/cos x))

(defn sin
  "Sine."
  [x]
  (mat/sin x))

;(defn exp-xi
;  "For some real x, calculates e^(xi)."
;  [x]
;  (math.exp (math.complex. 0 x)))

(defn exp-xi
  "For some real x, calculates e^(xi)"
  [x]
  (c/pow Math/E (c/complex-number 0 x)))

;(defn- radians-to-degrees
;  "Converts a number in radians to a number in degrees."
;  [x]
;  (-> x (divide (.-tau math)) (multiply 360)))

(defn to-phase
  "for some complex a+bi, returns the phase"
  [x]
  (Math/atan2 (compl/imag x) (compl/real x)))

;(defn to-phase
;  "For some complex a+bi, returns the phase."
;  [x]
;  (matrix-safe (comp radians-to-degrees math.arg) x))

;Matrix operations
(defn det
  "Returns the determinant of some matrix"
  [mat]
  (mat/det mat))

;(defn- rand-2D-complex
;  "Creates a 2D math.js matrix with elements equal to a random complex number with real part between positive and minus real-bound, and with imaginary part between positive and minus imag-bound."
 ; ([m n bound]
 ;  (rand-2D-complex m n bound bound))
;  ([m n real-bound imag-bound]
;   (letfn [(rand-n [bound] (-> bound (* 2) inc (- bound) rand-int))
;           (rand-cmp [] (math.complex. (rand-n real-bound)
;                                       (rand-n imag-bound)))
;           (rand-1D [dim] (->> rand-cmp
;                               (repeatedly dim)
;                               to-matrix))]
;     (->> #(rand-1D n)
;          (repeatedly m)
;          to-matrix))))

;(defn- mat=?-1
;  "Internal function for testing whether two matrices are equal (i.e. dimensions are the same, and corresponding elements are equal)."
;  ([mat01 mat02]
;   (if (some false? (math.equal (.size mat01) (.size mat02)))
;     false
;     (->> (math.equal mat01 mat02)
;          nested-to-vec
;          flatten
;          (every? true?))))
;  ([mat01 mat02 & more]
;   (and (mat=? mat01 mat02) (apply mat=? mat02 more))))

(defn mat=? [x y]
  (mat/equals x y))

(defn square?
  "Predicate test for whether a 2D collection is square (i.e. its height and width are equal)."
  [coll]
  (mat/square? coll))

(defn complex-conjugate
  "Returns the complex conjugate of some number. The complex conjugate of a+bi is a-bi."
  [x]
  (compl/complex (compl/real x) (- (compl/imag x))))

(defn conjugate-transpose
  "The result of tranposing a 2D matrix whose elements have been complex conjugated."
  [m]
  (compl/hermitian-transpose m))

;(defn unitary? ;needs to be properly implemented
;  "Predicate test for whether a given collection, when converted to a matrix, is unitary. A matrix M is unitary if M multiplied by its conjugate transpose and its conjugate transpose multiplied by it are both equal to the identity matrix."
;  [coll]
;  (let [mat (nested-to-matrix coll)
;        [m n] (.size mat)
;        id-mat (math.eye m n)
;        conj-trans (conjugate-transpose mat)]
;    (mat=? id-mat
;           (nested-to-matrix (round (multiply mat conj-trans) 5))
;           (nested-to-matrix (round (multiply conj-trans mat) 5)))))

(defn unitary?
  "Predicate test for whether a given collection, when converted to a matrix, is unitary. A matrix M is unitary if M multiplied by its conjugate transpose and its conjugate transpose multiplied by it are both equal to the identity matrix."
  [m]
  (let [id-map (mat/identity-matrix (mat/dimension-count m 1))]
    (mat/equals id-map (multiply m (conjugate-transpose m))))
  )

;Helpful utilities
(defn to-string
  [x]
  (.toString x))

;(defn eval-math-string
;  "Evaluates a string as a mathematical expressions. For example (eval-math-string \"e^(pi*i)\") should return -1."
;  [s]
;  (round (math.eval s) 9))

(defn math-split
  [m]
  (re-seq #"\*{2}|\d{2}|pi|e|i|[^*]|[+*]|[-*]|[/*]|\*|\d+|\w+" (clojure.string/replace m #"\^" "**")))

(defn convert
  [s]
  (try
    (Long/parseLong s)
    (catch NumberFormatException _
      (symbol s))))

;(defmacro eval-math-string
;  "Evaluates a string as a mathematical expressions. For example (eval-math-string \"e^(pi*i)\") should return -1."
;  [s]
;  `(ic/$= ~@(map convert (math-split s)
;    )))

(defn eval-math-string
  [s]
  (eval (cons 'incanter.core/$= (map read-string (math-split s)))))


;(defn eval-math-string
;  "Evaluates a string as a mathematical expressions. For example (eval-math-string \"e^(pi*i)\") should return -1."
;  [s]
;  (ic/$= (re-seq #"\*{2}|\d{2}|pi|e|i|[^*]|[+*]|[-*]|[/*]|\*|\d+|\w+" (replace s #"\^" "**"))))


;#"\*{2}|pi|e|i|[^*]|[+*]|[-*]|[/*]|\*|\d+\w+"

;Eigenvalues
(defn complex?
  [x]
  (compl/complex? x))

;(defn- coll->bruenner-matrix
;  [coll]
;  (w/postwalk (fn [form]
;                (cond
;                  (sequential? form) (to-array form)
;                  (number? form) (array form 0)
;                  (complex? form) (array (.-re form) (.-im form))))
;              coll))

;(defn- bruenner-matrix->coll
;  [mat]
;  (w/prewalk (fn [form]
;               (if (array? form)
;                 (let [[real imag :as v] (vec form)]
;                   (if (and (number? real) (number? imag))
;                     (math.complex. real imag)
;                     v))
;                 form))
;             mat))

(defn eigenvalues [coll])
;(defn eigenvalues ;;needs to be properly implemented
;  [coll]
;  {:pre [(= 4 (count coll))
;         (every? (partial = 4) (map count coll))
;         (every? (some-fn number? complex?) (flatten coll))]}
;  (let [mat (coll->bruenner-matrix coll)]
;    (bruenner-matrix->coll
;      (js/eigenvalues4 mat))))