(ns tiltontec.its-alive.model-base-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            ;[tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            [tiltontec.its-alive.model-base :refer :all :as md]))

(deftest puh-leaze
  (is (= 4 (+ 2 2))))



