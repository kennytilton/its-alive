(ns tiltontec.its-alive.cell-types-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            ))

(deftest nada-much
  (is (isa? ::cty/c-formula ::cty/cell)))
