(ns tiltontec.its-alive.evaluate-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.constructor :refer :all]
            [tiltontec.its-alive.evaluate :refer [cell-read]]
            ))

(deftest test-input
  (let [c (c-in 42 :slot :bingo)]
    (is (ia-type? @c ::cty/cell))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (c-valid? c))
    (is (nil? (c-model c)))
    (is (= :bingo (c-slot c) (c-slot-name c)))
    (is (= (cell-read c) 42))
    ))

(deftest t-formula
  (let [c (c? (+ 40 2))]
    (is (isa? ia-types ::cty/c-formula ::cty/cell))
    (is (ia-type? @c ::cty/cell))
    (is (ia-type? @c ::cty/c-formula))
    (is (= (c-value-state c) :unevaluated))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (not (c-valid? c)))
    (is (nil? (c-model c)))
    (println :readddd (cell-read c))
    (is (= (cell-read c) 42))
    ))


(deftest t-formula-2
  (let [b (c-in 2)
        c (c? (+ 40 (cell-read b)))
        d (c? (/ (cell-read c)
                 (cell-read b)))]
    (is (= (cell-read d) 21))
    (is (= (cell-read c) 42))
    (is (= (cell-read b) 2))
    ))

    
