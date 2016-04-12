(ns tiltontec.its-alive.constructor-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.constructor :refer :all]
            ))

(ns-unmap *ns* 'c-fn-var)

(deftest test-input
  (let [in1 (make-cell 
             :slot :mol
             :value 42)]
    (is (isa? ia-types (type @in1) :cell))
    (is (= (c-value in1) 42))
    (is (= (c-value-state in1) :valid))
    (is (= #{} (c-callers in1)))
    (is (c-input? in1))
    (is (nil? (c-model in1)))
    (is (= :mol (c-slot in1)))
    ))


(deftest test-c-in
  (let [in1 (c-in 42)] 
    (is (isa? ia-types (type @in1) :cell))
    (is (= (c-value in1) 42))
    (is (= (c-value-state in1) :valid))
    (is (= #{} (c-callers in1)))
    (is (c-input? in1))
    (is (nil? (c-model in1)))
    ))

(deftest test-c-formula
  (let [c (c? (+ 40 2))] 
    (is (isa? ia-types (type @c) :c-formula))
    (is (fn? (c-rule c)))
    (is (= (c-value c) unevaluated))
    (is (= (c-value-state c) :unevaluated))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (nil? (c-model c)))
    ))




