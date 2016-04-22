
(ns tiltontec.its-alive.integrity-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))

(deftest integ-1
  (is (= 4 (+ 2 2))))

(defn obsdbg []
  (fn-obs (trx :obsdbg slot new old (type-of c))))


(deftest obs-setf
  (cells-init)
  (is (zero? @+pulse+))
  (binding [*dp-log* true]
    (let [alarm (c-in :undefined :obs (obsdbg))
          act (c-in nil :obs (obsdbg))
          loc (c?+ [:obs (fn-obs (trx :loc-obs-runs!!!!)
                                 
                                 (when-not (= new :missing)
                                   (assert (= @+pulse+ 2))
                                   (c-reset-next! alarm
                                                  (case new
                                                    :home :off
                                                    :away :on
                                                    (err format "unexpected loc %s" new)))))]
                   (case (c-get act)
                     :leave :away
                     :return :home
                     :missing))
          alarm-speak (c?+ [:obs (fn-obs 
                                  (trx :alarm-speak (c-get act) :sees (c-get alarm) (c-get loc))
                                  (is (= (c-get alarm) (case (c-get act)
                                                         :return :off
                                                         :leave :on
                                                         :undefined)))
                                  (is (= +pulse+
                                         (c-pulse act)
                                         (c-pulse loc)
                                         (c-pulse c))))]
                           (str "alarm-speak sees act " (c-get act)))]
      (is (= (c-get alarm) :undefined))
      (is (= 1 @+pulse+))
      (is (= (c-get loc) :missing))
      (is (= 1 @+pulse+))

      (c-reset! act :leave)
      (is (= 3 @+pulse+))
      )))

;; -----------------------------------------------------------------


(deftest obs-setf-bad-caught
  (cells-init)

  (let [alarm (c-in :undefined :obs (obsdbg))
        act (c-in nil :obs (obsdbg))
        loc (c?+ [:obs (fn-obs (trx :loc-obs-runs!!!!)
                                (is (thrown-with-msg?
                                     Exception
                                     #"c-reset!> change"
                                     (c-reset! act :leave)))
                               (when-not (= new :missing)
                                 (c-reset-next! alarm (case new
                                                   :home :off
                                                   :away :on
                                                   (err format "unexpected loc %s" new)))))]
                 (case (c-get act)
                   :leave :away
                   :return :home
                   :missing))
        alarm-speak (c?+ [:obs (fn-obs 
                                (trx :alarm-speak (c-get act) :sees (c-get alarm) (c-get loc))
                                (is (= (c-get alarm) (case (c-get act)
                                                       :return :off
                                                       :leave :on
                                                       :undefined)))
                                (is (= +pulse+
                                       (c-pulse act)
                                       (c-pulse loc)
                                       (c-pulse c))))]
                         (str "alarm-speak sees act " (c-get act)))]
    (is (= (c-get alarm) :undefined))
    (is (= 1 @+pulse+))
    (is (= (c-get loc) :missing))
    (is (= 1 @+pulse+))
    
   
    ))

;; --------------------------------------------------------

(deftest see-into-fn 
  (let [sia (c-in 0)
        rsic (atom false)
        sic (c? (reset! rsic true)
                (+ 42 (c-get sia)))
        fsia #(c-get sia)
        sib (c? (or (+ 1 (fsia))
                    (c-get sic)))]
    (is (= (c-get sib) 1))
    (is (= (:useds @sib) #{sia}))
    (is (not @rsic))
    (c-reset! sia 1)
    (is (= 2 (:value @sib)))
    (is (= (c-get sib) 2))))


(deftest no-prop-no-obs
  (let [sia (c-in 0)
        obs (atom false)
        sib (c?+ [:obs (fn-obs (reset! obs true))]
                 (if (even? (c-get sia))
                   42
                   10))
        run (atom false)
        sic (c? (reset! run true)
                (/ (c-get sib) 2))]
    (is (= (c-get sib) 42))
    (is (= (c-get sic) 21))
    (is @obs)
    (is @run)
    (dosync
     (reset! obs false)
     (reset! run false))
    (c-reset! sia 2)
    (is (= (c-get sib) 42))
    (is (= (c-get sic) 21))
    (is (not @obs))
    (is (not @run))))
    
        
        
        
