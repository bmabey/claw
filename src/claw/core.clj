(ns claw.core
  "Wrappers for Parallel Colt.
   Many of these functions return views of matrices.  A view is a reindexing of a given matrix.
   So the underlying structure is not changed nor copied.  Since no copying or creation is required
   most views are created in O(1).  The only exception is the sort functions which require, typically,
   O(nlogn) time.  Keep in mind that if you mutate a view you will be mutating the original data!"
  (:import
   cern.colt.matrix.AbstractMatrix
   [cern.colt.matrix.tdouble.impl
    DenseDoubleMatrix1D DenseColumnDoubleMatrix2D
    SelectedDenseDoubleMatrix1D SelectedDenseColumnDoubleMatrix2D]
   [cern.colt.matrix.tdouble DoubleMatrix1D DoubleMatrix2D]
   [cern.colt.function.tdouble DoubleFunction DoubleDoubleFunction]
   cern.colt.matrix.tdouble.algo.DoubleSorting
   cern.jet.math.tdouble.DoubleFunctions)
   (:use [clojure.core.incubator :only [defmacro-]]))

(def double-array-class (class (double-array [0.0])))
(def double2d-array-class (class (into-array [ (double-array [0.0]) (double-array [0.0])])))

(defn doubles2D-array? [obj]
  (= (class obj) double2d-array-class))

(defn double-arrays
  "Converts any 2d data structure into a 2D array of doubles (primitives)"
  [xs]
  (if (doubles2D-array? xs)
    xs
    (into-array (map double-array xs))))

(defn matrix
  "Creates a DoubleMatrix2D from a list of Clojure data strucrues"
  [rows]
  (DenseColumnDoubleMatrix2D. ^"[[D" (double-arrays rows)))

(defn row-vector [seq]
  (DenseDoubleMatrix1D. (double-array seq)))

(defn transpose
  "Returns a view of the transposition of M."
  [^DoubleMatrix2D M]
  (.viewDice M))

(defn size [^AbstractMatrix x]
  (.size x))

(defprotocol Arrayizable
  (to-array [x] "Returns the double array version of the vector or matrix."))

;; TODO: DRY up with macro...
(extend-protocol Arrayizable
  DenseColumnDoubleMatrix2D
  (to-array [^DenseColumnDoubleMatrix2D x]
    (.toArray x))

  DenseDoubleMatrix1D
   (to-array [^DenseDoubleMatrix1D x]
    (.toArray x))

  SelectedDenseDoubleMatrix1D
  (to-array [^SelectedDenseDoubleMatrix1D x]
    (.toArray x)))

(defn to-vec
  "Converts the given matrix into a clojure vector"
  [x]
  (vec (to-array x))) ;; TODO: use vector-of or similar to store the primitives?


;; Better name?  I'd like to be able to say (v 2) or (nth v 2) but that would require extending or wrapping the class
(defn vget [^DenseDoubleMatrix1D vector n]
  (.get vector n))

(defn to-seq
  "Returns a lazy seq of the given vector. Note that this will incur the Double tax.
   This should not be a primary form of computation with vectors and matrices."
  [^DenseDoubleMatrix1D x]
  (map #(vget x %) (range (size x))))

(defn cols->matrix [cols]
  (-> cols matrix transpose))

(declare col-sel->num row-sel->num)

(defn column
  "Returns a view of the column at index col-num."
  [^DoubleMatrix2D M col-selector]
  (.viewColumn M (int (col-sel->num M col-selector))))

;;TODO: use something like defalias (from c.c.def 1.2) to copy metadata
(def col column)

(defn row
  "Returns a view of the row at index row-num."
  [^DoubleMatrix2D M row-selector]
  (.viewRow M (int (row-sel->num M row-selector))))

(defn num-columns
  "Returns the number of columns in M"
  [^DoubleMatrix2D M]
  (.columns M))

;;TODO: use something like defalias (from c.c.def 1.2) to copy metadata
(def num-cols num-columns)

(defn num-rows
  "Returns the number of rows in M"
  [^DoubleMatrix2D M]
  (.rows M))

(defn dimension
  "Returns the physical dimensions of the matrix as [num-rows num-cols].
   N.B: Not the dimenension of the basis."
  [M]
  [(num-rows M) (num-cols M)])

(defn columns
  "Returns a lazy seq of the columns of M."
  [M]
  (map (partial col M) (range (num-cols M))))

;;TODO: use something like defalias (from c.c.def 1.2) to copy metadata
(def cols columns)

(defn rows
  "Returns a lazy seq of the columns of M."
  [M]
  (map (partial row M) (range (num-rows M))))

(defn col-sel->num [x selector]
  (if (number? selector)
    selector
    (case selector
      :last (dec (num-cols x))
      :first 0
      (throw (IllegalArgumentException. (str "Invalid column selector: " selector))))))

(defn row-sel->num [x selector]
  (if (number? selector)
    selector
    (case selector
      :last (dec (num-rows x))
      :first 0
      (throw (IllegalArgumentException. (str "Invalid row selector: " selector))))))

(defn vreverse [^DenseDoubleMatrix1D vector]
  (.viewFlip vector))

(declare size)

(defn subvector
  "Same as clojure's subvec (so, start is inclusive and end is exclusive).
  Returns a view of 'vector' between the given 'start' and 'end' indices."
  ([vector start]
     (subvector vector start (size vector)))
  ([^DenseDoubleMatrix1D vector start end]
     (.viewPart vector (int start) (- end start))))

(defn vtake
  "Returns the first 'n' elements from the vector."
  [n ^DenseDoubleMatrix1D vector]
  (.viewPart vector 0 (int n)))

(defn select-subset
  "Returns a subset view matrix of 'M' consisting of the rows in 'rows-indexes' and 'col-indexes'."
  [^DoubleMatrix2D M row-indexes col-indexes]
  (.viewSelection M  (int-array row-indexes) (int-array col-indexes)))

(defn select-cols
  "Returns a subset view matrix of 'M' consisting of the columns in 'col-indexes'."
  [M col-indexes]
  (select-subset M (range (num-rows M)) col-indexes))

(defn select-rows
  "Returns a subset view matrix of 'M' consisting of the rows in 'rows-indexes'."
  [M row-indexes]
  (select-subset M row-indexes (range (num-cols M))))

(defn sort-rows-by-column
  "Returns a sorted view, in ascending order, of the rows by the given column selector."
  [^DoubleMatrix2D M col-sel]
  (.viewSorted M (int (col-sel->num M col-sel))))

(defn flip-rows [^DoubleMatrix2D M]
  (.viewRowFlip M))

(defprotocol ElementWise
  (map-elements [x f] [x y f]
    "Like clojure's map but is eager and intended to operate without the seq abstraction (e.g. arrays, matrices).")
  (map-elements! [x f] [x y f]
    "Same as emap but the mapping is destructive and operates on 'x' in place.")
  (map-reduce-elements [x map-fn reduce-fn] [x y map-fn reduce-fn]
    "Maps and then reduces. This is needed since map-elements is not lazy."))

(defprotocol Summable
  (sum [x] "Returns the sum of the elements"))

;; Note: These will return boxed versions. :( http://groups.google.com/group/clojure/browse_thread/thread/6acff496005d7731
(extend-protocol Summable
  DenseColumnDoubleMatrix2D
  (sum [^DenseColumnDoubleMatrix2D x]
    (.zSum x))

  DenseDoubleMatrix1D
   (sum [^DenseDoubleMatrix1D x]
    (.zSum x))


  SelectedDenseDoubleMatrix1D
  (sum [^SelectedDenseDoubleMatrix1D x]
    (.zSum x)))

(defn- double-fn [f]
  (if (instance? DoubleFunction f)
    f
    (reify DoubleFunction
      (apply [_ val] (f val)))))

(defn- double-double-fn [f]
  (if (instance? DoubleDoubleFunction f)
    f
    (reify DoubleDoubleFunction
      (apply [_ x-val y-val] (f x-val y-val)))))

(extend-protocol ElementWise
  DenseColumnDoubleMatrix2D
  (map-elements
    ([^DenseColumnDoubleMatrix2D x f]
       (.assign ^DenseColumnDoubleMatrix2D (.copy x) ^DoubleFunction (double-fn f)))
    ([x ^DenseColumnDoubleMatrix2D y f]
       (.assign ^DenseColumnDoubleMatrix2D (.copy x) y ^DoubleDoubleFunction (double-double-fn f))))
  (map-elements!
    ([^DenseColumnDoubleMatrix2D x f]
       (.assign x ^DoubleFunction (double-fn f)))
    ([^DenseColumnDoubleMatrix2D x ^DenseColumnDoubleMatrix2D y f]
       (.assign x y ^DoubleDoubleFunction (double-double-fn f))))
  (map-reduce-elements
    ([^DenseColumnDoubleMatrix2D x map-fn reduce-fn]
       (.aggregate x ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleFunction (double-fn map-fn)))
    ([^DenseColumnDoubleMatrix2D x ^DenseColumnDoubleMatrix2D y map-fn reduce-fn]
       (.aggregate x y ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleDoubleFunction (double-double-fn map-fn))))

  DenseDoubleMatrix1D
  (map-elements
    ([^DenseDoubleMatrix1D x f]
       (.assign ^DenseDoubleMatrix1D (.copy x) ^DoubleFunction (double-fn f)))
    ([x ^DenseDoubleMatrix1D y f]
       (.assign ^DenseDoubleMatrix1D (.copy x) y ^DoubleDoubleFunction (double-double-fn f))))
  (map-elements!
    ([^DenseDoubleMatrix1D x f]
       (.assign x ^DoubleFunction (double-fn f)))
    ([^DenseDoubleMatrix1D x ^DenseDoubleMatrix1D y f]
       (.assign x y ^DoubleDoubleFunction (double-double-fn f))))
  (map-reduce-elements
    ([^DenseDoubleMatrix1D x map-fn reduce-fn]
       (.aggregate x ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleFunction (double-fn map-fn)))
    ([^DenseDoubleMatrix1D x ^DenseDoubleMatrix1D y map-fn reduce-fn]
       (.aggregate x y ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleDoubleFunction (double-double-fn map-fn))))

  SelectedDenseDoubleMatrix1D
  (map-elements
    ([^SelectedDenseDoubleMatrix1D x f]
       (.assign ^SelectedDenseDoubleMatrix1D (.copy x) ^DoubleFunction (double-fn f)))
    ([x ^SelectedDenseDoubleMatrix1D y f]
       (.assign ^SelectedDenseDoubleMatrix1D (.copy x) y ^DoubleDoubleFunction (double-double-fn f))))
  (map-elements!
    ([^SelectedDenseDoubleMatrix1D x f]
       (.assign x ^DoubleFunction (double-fn f)))
    ([^SelectedDenseDoubleMatrix1D x ^SelectedDenseDoubleMatrix1D y f]
       (.assign x y ^DoubleDoubleFunction (double-double-fn f))))
  (map-reduce-elements
    ([^SelectedDenseDoubleMatrix1D x map-fn reduce-fn]
       (.aggregate x ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleFunction (double-fn map-fn)))
    ([^SelectedDenseDoubleMatrix1D x ^SelectedDenseDoubleMatrix1D y map-fn reduce-fn]
       (.aggregate x y ^DoubleDoubleFunction (double-double-fn reduce-fn) ^DoubleDoubleFunction (double-double-fn map-fn)))))

(defn emap
  ([f x] (map-elements x f))
  ([f x y] (map-elements x y f)))

(defn emap!
  ([f x] (map-elements! x f))
  ([f x y] (map-elements! x y f)))

(defn map-reduce
  (^double [map-fn reduce-fn x y]
     (map-reduce-elements x y map-fn reduce-fn))
  (^double [map-fn reduce-fn x]
     (map-reduce-elements x map-fn reduce-fn)))

(defn ereduce ^double [f x]
  ;; I'm using the colt identity fn to avoid wrapping from occuring
  (map-reduce (DoubleFunctions/identity) f x))

;; (with-meta (double-array ~how-many) {:tag 'doubles})

(defmacro map-to-doubles
  [how-many idx expr]
  `(let [vals# (double-array ~how-many)]
         (dotimes [~idx ~how-many]
           (aset vals# ~idx ~expr))
         vals#))

(defn reduce-rows [f m]
  ;; TODO: even with the array creation the Double tax is still happening until we move to 1.3
  (row-vector (map-to-doubles (num-rows m) i (ereduce f (row m i)))))

(defn reduce-cols [f m]
  (row-vector (map-to-doubles (num-cols m) i (ereduce f (col m i)))))

(defn map-rows [f m]
  (row-vector (map-to-doubles (num-rows m) i (double (f (row m i)))))) ;; double needed to avoid reflection

(defn map-cols [f m]
  (row-vector (map-to-doubles (num-cols m) i (double (f (col m i)))))) ;; double needed to avoid reflection

(defn sort-rows-by-column-desc
  "Returns a sorted view, in descending order, of the rows by the given column selector."
  [M col-sel]
  (-> M (sort-rows-by-column col-sel) flip-rows))

;;TODO: use something like defalias (from c.c.def 1.2) to copy metadata
(def sort-rows-by-col sort-rows-by-column)

;;TODO: use something like defalias (from c.c.def 1.2) to copy metadata
(def sort-rows-by-col-desc sort-rows-by-column-desc)

(defn sort-rows-by
  "Returns a sorted view, in ascending order, of 'M' by 'f'. 'f' takes a row and returns a double."
  [^DoubleMatrix2D M f]
  (.sort (DoubleSorting/quickSort) M ^"[D" (to-array (map-rows f M))))

(defn sort-rows-by-desc
  "Returns a sorted view, in descending order, of the rows by the given column index."
  [^DoubleMatrix2D M f]
  (-> M (sort-rows-by f) flip-rows))

(defprotocol Arithmetic
  (add  [a] [a b])
  (sub  [a] [a b])
  (mult [a] [a b])
  (div  [a] [a b]))

(defmacro- extend-matrices [& body]
  `(do
     (extend-type DenseDoubleMatrix1D ~@body)
     (extend-type SelectedDenseDoubleMatrix1D ~@body)
     (extend-type DenseColumnDoubleMatrix2D ~@body)))

;; The cern functions return primitives and are unchecked.
(defonce colt-plus (DoubleFunctions/plus))
(defonce colt-minus (DoubleFunctions/minus))
(defonce colt-mult (DoubleFunctions/mult))
(defonce colt-div (DoubleFunctions/div))
(defonce colt-abs (DoubleFunctions/abs))


(extend-matrices
 Arithmetic
 (add [a b]
      (emap colt-plus a b))
 (sub [a b]
      (emap colt-minus a b))
 (mult [a b]
       (emap colt-mult a b))
 (div [a b]
      (emap colt-div a b)))

(defn ereductions [f vector]
  (row-vector
   (let [how-many (int (size vector))
         vals (doto (double-array how-many) (aset 0 (double (vget vector 0))))]
     (loop [i (int 1)]
       (when (< i how-many)
         (aset vals i
               (double (f (aget vals (unchecked-dec i)) (vget vector i))))
         (recur (unchecked-inc i))))
      vals)))

(defn mean ^double [x]
  (if (== 0 (size x))
    0.0
    (/ (sum x) (size x))))

(defn conj-cols [^DoubleMatrix2D a ^DoubleMatrix2D b]
  (let [[a-rows a-cols] (dimension a)
        [b-rows b-cols] (dimension b)]
    (when (not= a-rows b-rows)
      (throw (IllegalArgumentException.
              (format "Number of rows must be identical in order to conj-cols. (num-rows a: %o, num-rows b: %o)" a-rows b-rows))))
    (let [x (.like a a-rows (+ a-cols b-cols))]
      (doto (.viewPart x 0 0 a-cols a-rows) (.assign a))
      (doto (.viewPart x 0 a-cols b-rows b-cols) (.assign b))
      x)))

(defn conj-col [^DoubleMatrix2D a ^DoubleMatrix1D b]
  (let [[a-rows a-cols] (dimension a)]
    (when (not= a-rows (size b))
      (throw (IllegalArgumentException.
              (format "Number of rows must be identical in order to conj-col. (num-rows a: %o, num-rows b: %o)" a-rows (size b)))))
    (let [x (.like a a-rows (inc a-cols))]
      (doto (.viewPart x 0 0 a-rows a-cols) (.assign a))
      (doto (.viewPart x 0 a-cols a-rows 1) (.assign ^"[D" (to-array b)))
      x)))

(defn weighted-mean ^double [x weights]
  (if (== 0 (size x))
    0.0
    (/ (map-reduce colt-mult colt-plus x weights) (sum weights))))

(defn map-mean
  "Maps with 'f' and takes the mean of the resulting values. Internaly a map-reduce is used for efficiency."
   (^double [f x y]
      (/ (map-reduce f colt-plus x y) (size x)))
   (^double [f x]
      (/ (map-reduce f colt-plus x) (size x))))

;; TODO: using the matrix factories would be faster...
(defn rand-matrix
  ([m] (rand-matrix m m))
  ([m n]
      (let [rand-row #(take n (repeatedly rand))]
        (matrix (take m (repeatedly rand-row))))))
