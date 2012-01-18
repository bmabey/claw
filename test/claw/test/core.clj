(ns claw.test.core
  (:use [claw.core])
  (:use clojure.test midje.sweet))
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

(deftest to-seq-test
  (fact
    (to-seq (row-vector [1 2 3 4])) => [1 2 3 4]))

(deftest matrix-test
  (fact
    (let [my-rows [[1.0 2.0 3.0]
                   [4.0 5.0 6.0]]]
      (map (comp vec to-array) (cols (matrix my-rows))) => [[1.0 4.0] [2.0 5.0] [3.0 6.0]])))

(deftest cols->matrix-test
  (fact
    (cols->matrix [[1 2 3] [4 5 6]]) => (matrix [[1 4]
                                                 [2 5]

                                                 [3 6]])))

(deftest rows-test
  (fact
    (let [my-rows [[1.0 2.0 3.0]
                   [4.0 5.0 6.0]]]
      (map (comp vec to-array) (rows (matrix my-rows))) => my-rows)))

(deftest col-sel->num-test
  (fact
    (let [x (matrix [[1 2]
                     [3 4]])]
      (col-sel->num x 0) => 0
      (col-sel->num x :first) => 0
      (col-sel->num x :last) => 1
      ;; if we had metadata on matrix we could name the cols.. that would be nice..
      (col-sel->num x :blah) => (throws IllegalArgumentException))))

(deftest row-sel->num-test
  (fact
    (let [x (matrix [[1 2]
                     [3 4]])]
      (row-sel->num x 0) => 0
      (row-sel->num x :first) => 0
      (row-sel->num x :last) => 1
      ;; if we had metadata on matrix we could name the cols.. that would be nice..
      (row-sel->num x :blah) => (throws IllegalArgumentException))))

(deftest subvector-test
  (fact
    (let [a (row-vector [0 1 2 3 4 5])]
      (subvector a 2 5) => (row-vector [2 3 4])
      (subvector a 2) => (row-vector [2 3 4 5]))))

(deftest vtake-test
  (fact
    (let [a (row-vector [0 1 2 3 4 5])]
      (vtake 3 a) => (row-vector [0 1 2]))))

(deftest select-subset-test
  (fact
    (select-subset (matrix [[1 2 3]
                            [4 5 6]
                            [7 8 9]]) [0 2] [0 1]) => (matrix [[1 2]
                                                               [7 8]])))

(deftest select-cols-test
  (fact
    (select-cols (matrix [[1 2 3]
                          [4 5 6]
                          [7 8 9]]) [0 2]) => (matrix [[1 3]
                                                       [4 6]
                                                       [7 9]])))

(deftest emap-test
  (facts
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
     M => (matrix [[2.0 3.0 4.0]]))))

(deftest map-reduce-test
  (facts
    (map-reduce inc + (matrix [[1.0 2.0]
                               [3.0 4.0]])) => 14.0
    (map-reduce + *
                (row-vector [1.0 2.0 3.0])
                (row-vector [1.0 2.0 3.0])) => (* (+ 1 1) (+ 2 2) (+ 3 3))))


(deftest ereduce-test
  (facts
    (ereduce + (matrix [[1.0 2.0]
                        [3.0 4.0]])) => 10.0
    (ereduce + (row-vector [1.0 2.0 3.0])) => 6.0))

(deftest map-to-doubles-test
  (fact
    (map-to-doubles 5 i (identity i)) => (double-array-of [0.0 1.0 2.0 3.0 4.0])))


(deftest reduce-rows-test
  (fact
    (reduce-rows + (matrix [[1 2 3]
                            [0 5 0]])) => (row-vector [6 5])))

(deftest reduce-cols-test
  (fact
    (reduce-cols + (matrix [[1 2 3]
                            [0 5 0]])) => (row-vector [1 7 3])))

(deftest map-rows-test
  (fact
    (map-rows #(/ (+ (vget % 0) (vget % 1)) (vget % 2))
              (matrix [[1 2 3]
                       [3 5 2]])) => (row-vector [1 4])))

(deftest map-cols-test
  (fact
    (map-cols #(/ (+ (vget % 0) (vget % 1)) (vget % 2))
              (matrix [[1 2]
                       [3 8]
                       [2 2]])) => (row-vector [2 5])))


(deftest sort-rows-by-column-desc-test
  (fact
    (let [M (matrix [[1 2 3] [0 1 5] [3 2 10]])]
      (sort-rows-by-column M 0) => (matrix  [[0 1 5] [1 2 3] [3 2 10]])
      (sort-rows-by-column-desc M 0) => (matrix  [[3 2 10] [1 2 3] [0 1 5]]))))

(deftest sort-rows-by-desc-test
  (fact
    (sort-rows-by-desc (matrix [[1 2 3] [4 5 6] [0 2 3] [10 10 10]]) sum)
    =>  (matrix [[10 10 10] [4 5 6] [1 2 3] [0 2 3]])))

(deftest colt-arithmetic
  (facts
    (let [a (row-vector [1 2 3])
          b (row-vector [2 2 2])]
      (add a b) => (row-vector [3 4 5])
      (sub a b) => (row-vector [-1 0 1])
      (mult a b) => (row-vector [2 4 6])
      (div a b) => (row-vector [1/2 2/2 3/2]))))

(deftest ereductions-test
  (fact
    (ereductions + (row-vector [1 1 2 2 3 3])) => (row-vector [1 2 4 6 9 12])))

(deftest conj-cols-test
  (fact
    (let [a (matrix [[1 2]
                     [3 4]])
          b (matrix [[2]
                     [5]])]
      (conj-cols a b) => (matrix [[1 2 2]
                                  [3 4 5]])
      (conj-cols a (matrix [[1]])) => (throws IllegalArgumentException))))

(deftest conj-col-test
  (fact
    (let [a (matrix [[1 2]
                     [3 4]
                     [5 6]])
          b (row-vector [2 5 7])]
      (conj-col a b) => (matrix [[1 2 2]
                                 [3 4 5]
                                 [5 6 7]])
      (conj-col a (row-vector [2])) => (throws IllegalArgumentException))))


(deftest weighted-mean-test
  (fact
    (weighted-mean (row-vector [80 90]) (row-vector [20 30])) => 86.0))

(deftest map-mean-test
  (fact
    (map-mean inc (row-vector [1 2 3 4 5])) => 4.0
    (map-mean + (row-vector [1 2 3 4 5]) (row-vector [1 1 1 1 1])) => 4.0))