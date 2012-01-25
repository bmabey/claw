(ns claw.test.test-helpers)

(definterface IPrimitiveTester
  (getType [^int x])
  (getType [^long x])
  (getType [^float x])
  (getType [^double x])
  (getType [^Object x]))

(deftype PrimitiveTester []
  IPrimitiveTester
  (getType [this ^int x] :int)
  (getType [this ^long x] :long)
  (getType [this ^float x] :float)
  (getType [this ^double x] :double)
  (getType [this ^Object x] :object))


(defmacro primitive-type [x]
  `(.getType (PrimitiveTester.) ~x))

(defmacro pt [x]
  `(.getType (PrimitiveTester.) ~x))

(defn arglist-tag [var]
  (-> var meta :arglists first meta :tag))
