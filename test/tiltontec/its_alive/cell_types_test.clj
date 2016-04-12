(ns tiltontec.its-alive.cell-types-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all as cty]
            ))

(descendants ia-types ::cell)
(deftest nada-much
  (is (isa? ia-types ::cty/c-formula ::cty/cell)))
