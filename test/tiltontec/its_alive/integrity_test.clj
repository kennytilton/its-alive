
(ns tiltontec.its-alive.integrity-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))

(deftest integ-1
  (is (= 4 (+ 2 2))))
