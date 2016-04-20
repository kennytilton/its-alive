
(ns tiltontec.its-alive.lazy-cells-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            ))

(deftest solid-lazy
  (cells-init)
  (let [xo (atom 0)
        a (c-in 0)
        x (c?_ [:obs (fn-obs (swap! xo inc))]
               (+ (c-get a) 40))]
    (is (= unevaluated (:value @x)))
    (is (= 0 @xo))
    (is (= 40 (c-get x)))
    (is (= 1 @xo)) 
    (c-reset! a 100)
    (is (= 1 @xo))
    (is (= 40 (:value @x)))
    (is (= 140 (c-get x)))
    (is (= 2 @xo)) 
    ))

(deftest lazy-until-asked
  (cells-init)
  (let [xo (atom 0)
        xr (atom 0)
        a (c-in 0)
        x (c_? [:obs (fn-obs (swap! xo inc))]
               (swap! xr inc)
               (+ (c-get a) 40))]
    (is (= unevaluated (:value @x)))
    (is (= 0 @xo))
    (is (= 0 @xr))
    (is (= 40 (c-get x)))
    (is (= 1 @xo))
    (is (= 1 @xr)) 
    (c-reset! a 100)
    (is (= 2 @xo))
    (is (= 2 @xr))
    (is (= 140 (:value @x)))
    (is (= 140 (c-get x)))
    (is (= 2 @xo)) 
    (is (= 2 @xr)) 
    ))

(deftest optimize-when-value-t
  (cells-init)
  (let [xo (atom 0)
        xr (atom 0)
        a (c-in 0 :slot :aaa)
        x (c?+ [:slot :xxx
               :obs (fn-obs (swap! xo inc))
               :optimize :when-value-t]
              (swap! xr inc)
              (trx nil :reading-a!!!)
              (when-let [av (c-get a)]
                (when (> av 1)
                  (+ av 40))))]
    (is (nil? (c-get x)))
    (is (= #{a} (c-useds x)))
    (c-reset! a 1)
    (trx nil :reset-finished!!!!!!!!!!)
    (is (nil? (c-get x)))
    (is (= #{a} (c-useds x)))
    (trx nil :reset-2-beginning!!!!!!!!!!!!)
    (c-reset! a 2)
    (trx nil :reset-2-finished!!!!!!!!!!)
    (is (= 42 (c-get x)))
    (is (empty? (c-useds x)))
    (trx nil :useds (c-useds x))
    (is (empty? (c-callers x)))
    ))

