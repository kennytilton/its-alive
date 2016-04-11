(ns com.tiltontec.jellz.ns-test
  (:require [clojure.test :refer :all]
            [com.tiltontec.jellz.ns :refer :all :as ns]))

(deftest jz-loop-atom
  (is (= @ns/loop-max 10000000)))

#_
(deftest add-x-to-y-a-using-are
  (are [x y] (= 5 (add x y))
       2 3
       1 4
       3 2))

