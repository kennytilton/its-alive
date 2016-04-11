(ns tiltontec.its-alive.constructor-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]
            [tiltontec.its-alive.constructor :refer :all]))


(deftest test-input
  (let [in1 (make-cell 
             :slot :mol
             :value 42)]
    (is (isa? ia-types (type @in1) :cell))
    (is (= (c-value in1) 42))
    (is (= (c-value-state in1) :valid))
    (is (= #{} (c-callers in1)))
    (is (c-input? in1))))
