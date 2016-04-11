(ns tiltontec.its-alive.utility-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all]))

(deftest err-ok
  (is (thrown? Exception
               (err "boom")))
  (is (thrown-with-msg? Exception
                        #"oom"
               (err "boom"))))
#_
(deftest fifo-build
  (let [q (make-fifo-queue)]
    (is (fifo-empty? q))
    (is (nil? (fifo-peek q)))
    (is (nil? (fifo-pop q)))
    (is (empty? (fifo-data q)))
    (dosync
     (fifo-add q 1)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (fifo-empty? q)))
    (dosync
     (fifo-add q 1)
     (fifo-add q 2)
     (is (not (fifo-empty? q)))
     (is (= 1 (fifo-peek q)))
     (is (= 1 (fifo-pop q)))
     (is (= 2 (fifo-pop q)))
     (is (fifo-empty? q)))))
