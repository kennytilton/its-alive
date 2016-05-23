(ns tiltontec.its-alive.observer-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.integrity :refer [with-integrity]]
            [tiltontec.its-alive.evaluate :refer [c-get]]
            [tiltontec.its-alive.observer :refer [defobserver fn-obs]]
            [tiltontec.its-alive.cells :refer :all]
            ))

(set! *print-level* 3)

(deftest t-formula
  (let [bingo (atom false)
        c (c?+ [:slot :bingo
                :obs (fn-obs
                      (reset! bingo true))]
               (+ 40 2))]
    (is (ia-type? c ::cty/cell))
    (is (ia-type? c ::cty/c-formula))
    (is (= (c-value-state c) :unbound))
    (is (= #{} (c-callers c)))
    (is (= #{} (c-useds c)))
    (is (not (c-input? c)))
    (is (not (c-valid? c)))
    (is (nil? (c-model c)))
    (is (= (c-get c) 42))
    (is (= 42 @c)) ;; ie, optimized-away
    (is @bingo)
    ))

(def bingo2 (atom false))

(defobserver :bingo2 [nil][]
  ;; (trx nil :bingoo2222222222!!!!! me new-value old-value c) 
  (reset! bingo2 true))


(deftest test-input
  (let [c (c-in 42 :slot :bingo2)]
    (is (ia-type? c ::cty/cell))
    (is (= (c-value-state c) :valid))
    (is (= #{} (c-callers c)))
    (is (c-input? c))
    (is (c-valid? c))
    (is (nil? (c-model c)))
    (is (= :bingo2 (c-slot c) (c-slot-name c)))
    (is (= (c-get c) 42))
    (is @bingo2)
    ))


(deftest t-custom-obs
  (let [bobs (atom nil)
        b (c-in 2 :slot :bb
                :obs (fn-obs
                       (trx nil slot me new old)
                       (reset! bobs new)))
        cobs (atom nil)
        c (c?+ [:obs (fn-obs [slot me new old c]
                       (trx slot me new old)
                       (reset! cobs new))]
               (* 10 (c-get b)))]
    (dosync
     (is (= (c-get b) 2))
     (is (= @bobs 2))
     (is (= (c-get c) 20))
     ;; (is (= @cobs 20))
     ;; (c-reset! b 3)
     ;; (is (= 3 @bobs))
     ;; (is (= 30 (c-get c)))
     ;; (is (= 30 @cobs))
     )))



(def obj Object)

(defmacro get-obj []
  `obj)

(defmulti speak (fn [one two] [(type one)(type two)]))

(derive ::dog ::animal)
(derive ::cat ::animal)

(defmethod speak [::dog Object] [_ _]
  (println :woof))

(speak (atom nil :meta {:type ::dog}) 42)

(defmethod speak [::cat obj] [_ _]
  (println :meow))

(speak (atom nil :meta {:type ::cat}) 42)

;; (def gotten-obj (obj/get-obj))
