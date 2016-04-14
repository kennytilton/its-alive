(ns tiltontec.its-alive.observer-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.constructor :refer :all]
            [tiltontec.its-alive.integrity :refer [with-integrity]]
            [tiltontec.its-alive.evaluate :refer [cell-read c-reset!]]
            [tiltontec.its-alive.observer :refer [defobserver fn-obs]]
            ))

(set! *print-level* 3)

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

(deftest t-custom-obs
  (let [bobs (atom nil)
        b (c-in 2 :slot :bb
                :obs (fn-obs
                       (println slot me new old)
                       (reset! bobs new)))
        cobs (atom nil)
        c (c?+ [:obs (fn-obs [slot me new old c]
                       (println slot me new old)
                       (reset! cobs new))]
               (* 10 (cell-read b)))]
    (dosync
     (is (= (cell-read b) 2))
     (is (= @bobs 2))
     (is (= (cell-read c) 20))
     (is (= @cobs 20))
     (c-reset! b 3)
     (is (= 3 @bobs))
     (is (= 30 (cell-read c)))
     (is (= 30 @cobs))
     )))

