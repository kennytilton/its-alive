
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

(set! *print-level* 3)

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
  (cells-init)
  
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
    
    (c-reset! b 3)
    (is (= (cell-read d) 43/3))
    (is (= (cell-read c) 43))
    (is (= (cell-read b) 3))
    (is (= 2 @dct))
    (is (= 2 @cct))
    ))
    

    
;;; --- The Pentagram of Death: a hard use case for data integrity ------

#_
(alter-var-root #'*trx?* not)


(deftest pentagram-of-death
  ;;
  ;; Christened the Pentagram of Death by Phillip J Eby, this
  ;; is the use case that challenges an engine not to calculate
  ;; and observe transiently* inconsistent values when two different
  ;; dependency paths of one slot (here :ee) lead back to 
  ;; the same slot (:aa).
  ;;
  ;; * "Transiently" because the state change propagation eventually**
  ;;   gets :ee to the value consistent with the new state.
  ;; ** which is not
  ;;   good enough because observers may have already fired and produced
  ;;   side effects off the invalid state.
  ;;
  ;; The example is contrived but was contrived to replicate
  ;; a real dataflow failure that arose in my RoboCup simulation and 
  ;; prompted Cells 3 and the concept of data integrity.
  ;;
  ;; For the telling story behind the useless slot names :aa, :bb et al
  ;; please see: http://smuglispweeny.blogspot.com/2008/07/aa-bb-cc-and-dd.html
  ;;
  (cells-init)
  (let [run (atom {})
        obs (atom {})

        rset (fn []
               (swap! run empty)
               (swap! obs empty))

        logit (fn [log key]
                (swap! run assoc key
                       (inc (key @run 0))))

        logrun #(logit run %)

        cr (fn [c]
             (cell-read c))

        podobs (fn [slot me new old c]
                 (swap! obs assoc slot
                        (inc (slot @obs 0))))

        aa (c-in 1 :slot :aa :obs podobs)
        a7 (c-in 7 :slot :a7 :obs podobs)
        a70 (c?+ [:slot :a70 :obs podobs]
                 (logrun :a70)
                 (* 10 (cr a7)))
        bb (c?+ [:slot :bb :obs podobs]
                (logrun :bb)
                (cr aa))
        cc (c?+ [:slot :cc :obs podobs]
                (logrun :cc)
                (* 10 (cr aa)))
        dd (c?+ [:slot :dd :obs podobs]
                (logrun :dd)
                (if (even? (cr bb))
                  (* 10 (cr cc))
                  42))
        ee (c?+ [:slot :ee :obs podobs]
                (logrun :ee)
                (+ (cr a70) (cr bb) (* 10000 (cr dd))))
        verify-p-current (fn []
                           (is (= 2 (cr aa)))
                           (is (= 2 (cr bb)))
                           (is (= 20 (cr cc)))
                           (is (= 200 (cr dd)))
                           (is (= 2000072 (cr ee))))
        ]
    
    ;; next checks are just that the engine calculated well
    ;; and built a good dependency graph
    ;;
    (is (= 1 (cr aa)))
    (is (= 1 (cr bb)))
    (is (= 10 (cr cc)))
    
    (is (= 42 (cr dd)))
    (is (= 420071 (cr ee)))

    (is (= nil (c-useds aa)))
    (is (= #{:bb :cc} (c-slots aa :callers)))
    
    (is (= #{:aa} (c-slots bb :useds)))
    (is (= #{:dd :ee} (c-slots bb :callers)))
    
    (is (= #{:aa} (c-slots cc :useds)))
    (is (= #{} (c-slots cc :callers)))
    
    (is (= #{:bb} (c-slots dd :useds)))
    (is (= #{:ee} (c-slots dd :callers)))
    
    (is (= #{:a70 :bb :dd} (c-slots ee :useds)))
    (is (= #{} (c-slots ee :callers)))
    
    ;; ;; now we come to data integrity: when change happens
    ;; ;; do all and only those cells affected recalculate
    ;; ;; and reobserve and do so exactly once.
    ;; ;;
    (binding [*trx?* true]
      (rset)
      (trx reset!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
      (doseq [[k v] (seq @obs)]
        (trx obschk k v)
        (is (and (keyword? k)
                 (= 0 v))))
      (c-reset! aa (inc (cr aa)))

      ; check which rules ran
      ;
      (= #{:bb :cc :dd :ee} ;; but not a7
         (set (keys @run)))
      ;
      ; check those rules ran exactly once
      ;
      (doseq [[k v] (seq @run)]
        (is (and (keyword? k)
                 (= 1 v))))

      ; check which observers ran
      ;
      (= #{:aa :bb :cc :dd :ee}
         (set (keys @obs)))
      ;
      ; check those observers ran exactly once
      ;
      (doseq [[k v] (seq @obs)]
        (trx obschk k v)
        (is (and (keyword? k)
                 (= 1 v))))
    
      ; check that this time dd branched to use cc as well as bb
      ;
      (is (= #{:bb :cc} (c-slots dd :useds)))
    
      (verify-p-current)
    
      (c-reset! aa (inc (cr aa)))

      ; :aa hence :bb now odd so :dd goes back to 42
      ;
      (is (= 42 (cr dd)))
      ;
      ; ...and check dependency on :cc got pruned
      ;
      (is (= #{:bb} (c-slots dd :useds)))
      )))
