(ns tiltontec.its-alive.observer-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.constructor :refer :all]
            [tiltontec.its-alive.evaluate :refer [cell-read]]
            [tiltontec.its-alive.observer :refer [defobserver]]
            ))


;; (isa? (type nil)(type nil))

(def bingo (atom false))

(defobserver :bingo [nil][]
  (println :bingoooooooooooo!!!!! me new-value old-value c)
  (reset! bingo true))

(deftest t-formula
  (let [c (c?+ (:slot :bingo)
               (+ 40 2))]
    (is (ia-type? @c ::cty/cell))
    (is (ia-type? @c ::cty/c-formula))
    (is (= (c-value-state c) :unevaluated))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (not (c-valid? c)))
    (is (nil? (c-model c)))
    (is (= (cell-read c) 42))
    (is @bingo)
    ))

(def bingo2 (atom false))

(defobserver :bingo2 [nil][]
  (println :bingoo2222222222!!!!! me new-value old-value c)
  (reset! bingo2 true))

(deftest test-input
  (let [c (c-in 42 :slot :bingo2)]
    (is (ia-type? @c ::cty/cell))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (c-valid? c))
    (is (nil? (c-model c)))
    (is (= :bingo2 (c-slot c) (c-slot-name c)))
    (is (= (cell-read c) 42))
    (is @bingo2)
    ))

