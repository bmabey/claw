(ns claw.test.core
  (:use [claw.core])
  (:use midje.sweet))
;; midje checkers

(defn double-array-of [expected]
  (chatty-checker [actual]
                  (and
                   (= double-array-class (class actual))
                   (= (vec expected) (vec actual)))))

(defchecker double-array-of [expected]
  (checker [actual]
           (= (class actual) double-array-class)))
;; end of midje code


;; note- passing the var to facts does't do anything (yet) and is just
;; for documentation reasons.

(facts #'to-seq
  (to-seq (row-vector [1.0 2.0 3.0 4.0])) =>  [1.0 2.0 3.0 4.0])

(facts #'matrix
  (let [my-rows [[1.0 2.0 3.0]
                 [4.0 5.0 6.0]]]
    (map (comp vec to-array) (cols (matrix my-rows))) => [[1.0 4.0] [2.0 5.0] [3.0 6.0]]))

(facts #'cols->matrix
  (cols->matrix [[1 2 3] [4 5 6]]) => (matrix [[1 4]
                                               [2 5]

                                               [3 6]]))

(facts #'rows
  (let [my-rows [[1.0 2.0 3.0]
                 [4.0 5.0 6.0]]]
    (map (comp vec to-array) (rows (matrix my-rows))) => my-rows))

(facts #'col-sel->num
  (let [x (matrix [[1 2]
                   [3 4]])]
    (col-sel->num x 0) => 0
    (col-sel->num x :first) => 0
    (col-sel->num x :last) => 1
    ;; if we had metadata on matrix we could name the cols.. that would be nice..
    (col-sel->num x :blah) => (throws IllegalArgumentException)))

(facts #'row-sel->num
  (let [x (matrix [[1 2]
                   [3 4]])]
    (row-sel->num x 0) => 0
    (row-sel->num x :first) => 0
    (row-sel->num x :last) => 1
    ;; if we had metadata on matrix we could name the cols.. that would be nice..
    (row-sel->num x :blah) => (throws IllegalArgumentException)))

(facts #'subvector
  (let [a (row-vector [0 1 2 3 4 5])]
    (subvector a 2 5) => (row-vector [2 3 4])
    (subvector a 2) => (row-vector [2 3 4 5])))

(facts #'vtake
  (let [a (row-vector [0 1 2 3 4 5])]
    (vtake 3 a) => (row-vector [0 1 2])))

(facts #'select-subset
  (select-subset (matrix [[1 2 3]
                          [4 5 6]
                          [7 8 9]]) [0 2] [0 1]) => (matrix [[1 2]
                                                             [7 8]]))

(facts #'select-cols
  (select-cols (matrix [[1 2 3]
                        [4 5 6]
                        [7 8 9]]) [0 2]) => (matrix [[1 3]
                                                     [4 6]
                                                     [7 9]]))

(facts #'emap
  (emap inc (matrix [[1.0 2.0]
                     [3.0 4.0]])) => (matrix [[2.0 3.0]
                                              [4.0 5.0]])
                     (emap inc (row-vector [1.0 2.0 3.0])) => (row-vector [2.0 3.0 4.0])
                     (emap + (row-vector [1.0 2.0 3.0])
                           (row-vector [1.0 2.0 3.0])) => (row-vector [2.0 4.0 6.0])
                           (emap inc (col (matrix [[1.0 2.0 3.0]
                                                   [2.0 3.0 4.0]])
                                          0)) => (row-vector [2.0 3.0])
                                          (let [M  (matrix [[1.0 2.0 3.0]])]
                                            (emap! inc M) => (matrix [[2.0 3.0 4.0]])
                                            M => (matrix [[2.0 3.0 4.0]])))

(facts #'map-reduce
  (map-reduce inc + (matrix [[1.0 2.0]
                             [3.0 4.0]])) => 14.0
                             (map-reduce + *
                                         (row-vector [1.0 2.0 3.0])
                                         (row-vector [1.0 2.0 3.0])) => (double (* (+ 1 1) (+ 2 2) (+ 3 3))))


(facts #'ereduce
  (ereduce + (matrix [[1.0 2.0]
                      [3.0 4.0]])) => 10.0
                      (ereduce + (row-vector [1.0 2.0 3.0])) => 6.0)

(facts #'map-to-doubles
  (map-to-doubles 5 i (identity i)) => (double-array-of [0.0 1.0 2.0 3.0 4.0]))


(facts #'reduce-rows
  (reduce-rows + (matrix [[1 2 3]
                          [0 5 0]])) => (row-vector [6 5]))

(facts #'reduce-cols
  (reduce-cols + (matrix [[1 2 3]
                          [0 5 0]])) => (row-vector [1 7 3]))

(facts #'map-rows
  (map-rows #(/ (+ (vget % 0) (vget % 1)) (vget % 2))
            (matrix [[1 2 3]
                     [3 5 2]])) => (row-vector [1 4]))

(facts #'map-cols
  (map-cols #(/ (+ (vget % 0) (vget % 1)) (vget % 2))
            (matrix [[1 2]
                     [3 8]
                     [2 2]])) => (row-vector [2 5]))


(facts "about sorting rows by column"
  (let [M (matrix [[1 2 3] [0 1 5] [3 2 10]])]
    (sort-rows-by-column M 0) => (matrix  [[0 1 5] [1 2 3] [3 2 10]])
    (sort-rows-by-column-desc M 0) => (matrix  [[3 2 10] [1 2 3] [0 1 5]])))

(facts #'sort-rows-by-desc
  (sort-rows-by-desc (matrix [[1 2 3] [4 5 6] [0 2 3] [10 10 10]]) sum)
  =>  (matrix [[10 10 10] [4 5 6] [1 2 3] [0 2 3]]))

(facts "about colt arithmetic"
  (let [a (row-vector [1 2 3])
        b (row-vector [2 2 2])]
    (add a b) => (row-vector [3 4 5])
    (sub a b) => (row-vector [-1 0 1])
    (mult a b) => (row-vector [2 4 6])
    (div a b) => (row-vector [1/2 2/2 3/2])))

(facts #'ereductions
  (ereductions + (row-vector [1 1 2 2 3 3])) => (row-vector [1 2 4 6 9 12]))

(facts #'conj-cols
  (let [a (matrix [[1 2]
                   [3 4]])
        b (matrix [[2]
                   [5]])]
    (conj-cols a b) => (matrix [[1 2 2]
                                [3 4 5]])
    (conj-cols a (matrix [[1]])) => (throws IllegalArgumentException)))

(facts #'conj-col
  (let [a (matrix [[1 2]
                   [3 4]
                   [5 6]])
        b (row-vector [2 5 7])]
    (conj-col a b) => (matrix [[1 2 2]
                               [3 4 5]
                               [5 6 7]])
    (conj-col a (row-vector [2])) => (throws IllegalArgumentException)))


(facts #'weighted-mean
  (weighted-mean (row-vector [80 90]) (row-vector [20 30])) => 86.0)

(fact #'map-mean
  (map-mean inc (row-vector [1 2 3 4 5])) => 4.0
  (map-mean + (row-vector [1 2 3 4 5]) (row-vector [1 1 1 1 1])) => 4.0)