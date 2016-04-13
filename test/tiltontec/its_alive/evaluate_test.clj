(ns tiltontec.its-alive.evaluate-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.constructor :refer :all]
            [tiltontec.its-alive.integrity :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer [cell-read c-reset!]]
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
        cct (atom 0)
        dct (atom 0)
        c (c? (swap! cct inc)
              (+ 40 (cell-read b)))
        d (c? (swap! dct inc)
              (/ (cell-read c)
                 (cell-read b)))]
    (is (= (cell-read d) 21))
    (is (= (cell-read c) 42))
    (is (= (cell-read b) 2))
    (is (= 1 @dct))
    (is (= 1 @cct))
    (is (= 0 (count (c-useds b))))
    (is (= 2 (count (c-callers b))))
    (is (= 1 (count (c-useds c))))
    (is (= 1 (count (c-callers c))))
    (is (= 2 (count (c-useds d))))
    (is (= 0 (count (c-callers d))))

    ))

(def yowza (atom 0))
(defobserver :yowza [nil][]
  (trx yowza!!!!!!!!!!! slot new-value old-value)
  (reset! yowza new-value))

  

(deftest t-in-reset
  (reset! yowza 0)
  (is (= @yowza 0))
  (let [b (c-in 2 :slot :yowza)]
    (is (= 2 (cell-read b)))
    (is (= 2 @yowza))
    (c-reset! b 42)
    (is (= 42 (cell-read b)))
    (is (= 42 @yowza))))

(deftest t-formula-22
  (dosync
   (ref-set +pulse+ 1))
  
  (let [b (c-in 2 :slot :bb)
        cct (atom 0)
        dct (atom 0)
        c (c?+ [:slot :cc]
               (swap! cct inc)
               (+ 40 (cell-read b)))
        d (c?+ [:slot :dd]
               (swap! dct inc)
               (/ (cell-read c)
                  (cell-read b)))]
    (dosync
     (is (= (cell-read d) 21))
     (is (= (cell-read c) 42))
     (is (= (cell-read b) 2))
     (is (= 1 @dct))
     (is (= 1 @cct)))
    
    (dosync
     (with-integrity (:change :test2)
       (c-reset! b 3)
       (is (= (cell-read d) 43/3))
       (is (= (cell-read c) 43))
       (is (= (cell-read b) 3))
       (is (= 2 @dct))
       (is (= 2 @cct))
       ))))
    
