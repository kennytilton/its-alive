(ns tiltontec.its-alive.constructor-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.constructor :refer :all]
            ))
(isa? ia-types ::cty/cell ::cty/cell)
(type @(make-cell :slot :mol :value 42))
(isa? ia-types (type @(make-cell :slot :mol :value 42)) ::cty/cell)
(deftest test-input
  (let [c (make-cell 
             :slot :mol
             :value 42)]
    (is (isa? ia-types (type @c) ::cty/cell))
    (is (= (c-value c) 42))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (nil? (c-model c)))
    (is (= :mol (c-slot c)))
    ))

(deftest test-c-in
  (let [c (c-in 42)] 
    (is (isa? ia-types (type @c) ::cty/cell))
    (is (= (c-value c) 42))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (c-valid? c))
    (is (nil? (c-model c)))
    ))

(deftest test-c-in+
  (let [c (c-in 42 :slot :cool)] 
    (is (isa? ia-types (type @c) ::cty/cell))
    (is (c-ref? c))
    (is (= (c-value c) 42))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (nil? (c-model c)))
    (is (= :cool (c-slot c)(c-slot-name c)))
    ))

(deftest test-c-formula
  (let [c (c? (+ 40 2))] 
    (is (isa? ia-types (type @c) ::cty/c-formula))
    (is (fn? (c-rule c)))
    (is (= (c-value c) unevaluated))
    (is (= (c-value-state c) :unevaluated))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (nil? (c-model c)))
    ))

(deftest t-c?+
  (let [c (c?+ (:optimize false :slot :bingo)
               (println :cool)
               (+ 40 2))]
    (is (isa? ia-types (type @c) ::cty/c-formula))
    (is (c-ref? c))
    (is (fn? (c-rule c)))
    (is (= (c-value c) unevaluated))
    (is (= (c-value-state c) :unevaluated))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (nil? (c-model c)))
    (is (not (c-optimize c)))
    (is (= :bingo (c-slot c)(c-slot-name c)))))
    




