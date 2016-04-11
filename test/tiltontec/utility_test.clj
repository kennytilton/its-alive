(ns tiltontec.its-alive.utility-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all]))

(deftest fake-cl
  (is (= 42 (prog1 42 43 44)))
  (is (= 42 (b-when x (+ 21 21)
                    (print :cool)
                    x)))
  (is (nil? (b-when x false
                    42)))
  (are [lst] (= 42 (cl-find 42 lst))
       '(41 42 43)
       '(42 43 44)
       '(40 41 42))

  (is (= 42 (unless (= 2 3) 42)))
  (is (nil? (unless (= 2 2) 42))))

(deftest test-rmap-setf
  (let [x (ref {:value 0})]
    (is (= 0 (:value @x)))
    (dosync (rmap-setf (:value x) 42))
    (println :xxx x @x (:value @x))
    (is (= 42 (:value @x)))
    (is (let [j (dosync (rmap-setf (:value x) 43))]
                                        ;(println :xxx x @x (:value @x))          
                                        ;(println :j j (type j))
          (= 43 j)))
    (is (= 44 (dosync (rmap-setf (:value x) 44))))))

(deftest err-handling
  (is (thrown? Exception
               (err "boom")))
  (is (thrown-with-msg?
       Exception
       #"oom"
       (err "boom")))
  (is (thrown-with-msg?
       Exception
       #"Hi mom"
       (err format "Hi %s" 'mom)))
  (is (any-ref? (ref 42)))
  (are [x] (not (any-ref? x))
       nil
       42
       []
       (atom 42))
  (is (= "...... cool: 1, 2, 3\n:bingo\n"
      (with-out-str
        (binding [*trxdepth* 5]
          (wtrx (0 100 "cool" 1 2 3)
                (println :bingo))))))
  (is (= ". test: 3\n.. test: 2\n... test: 1\n.... test: 0\n"
         (with-out-str
           (wtrx-test 3))))
  )

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


#_
(deftest add-x-to-y-a-using-are
  (are [x y] (= 5 (add x y))
       2 3
       1 4
       3 2))

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
