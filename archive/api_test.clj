(ns com.tiltontec.jellz.api-test
  (:require [clojure.test :refer :all]
            [com.tiltontec.jellz.utils :refer :all :as ut]
            [com.tiltontec.jellz.ns :refer :all :as ns]
            [com.tiltontec.jellz.api :refer :all :as api]))

(set! *print-level* 2)

;; --- to change or not to change ----------------

(defmd ::thingy [])

(def-jzo-slots aa bb cc dd ee)

(deftest simple-no-change-no-prop
  (let [run (atom {})
        obs (atom {})

        rset (fn []
               (swap! run empty)
               (swap! obs empty))

        logit (fn [log key]
                (swap! run assoc key
                       (inc (key @run 0))))
        logrun #(logit run %)
        logran (fn [slot]
                 (slot @run 0))
        
        myobs (fn [slot me new old]
                 (swap! obs assoc slot
                        (inc (slot @obs 0))))

        wow  (mkjzo ::thingy
                    :aa (jzi 1)
                    :bb (jzf+ {:obs myobs}
                              (logrun :bb)
                              (max (aa me) 42))
                    :cc (jzf+ {:obs myobs}
                              (logrun :cc)
                              (+ 1 (bb me))))]
    (is (= 1 (aa wow)))
    (is (= 42 (bb wow)))
    (is (= 43 (cc wow)))
    (rset)
    (jz-set! wow :aa 5)
    (println :runs @run)
    (is (= 1 (logran :bb)))
    (is (= 42 (bb wow)))svr
    (is (zero? (logran :cc)))))

;; --- dynamic dependency ------------------------
;;
;; One thing "lifting" does not handle is dynamic
;; dependency, aka dependency on values used inside
;; a call to a function, ie, where dependency is
;; not lexically apparent. We test that here.
;;

(defn fn-using-aa-bb [me]
  (+ (aa me)
     (* 2 (bb me))))

(deftest jzo-test-into-function
  (let [cco (atom false)
        set-cco (fn [slot me new old]
                  (reset! cco new))
        
        wow  (mkjzo ::thingy
                 :aa 1
                 :bb (jzi 3)
                 :cc (jzf+ {:obs set-cco} (fn-using-aa-bb me))
                 )]
    (is (= 1 (aa wow)))
    (is (= 3 (bb wow)))
    (is (= 7 (cc wow)))
    (is (= 7 @cco))
    (is (= (cc wow) @cco))

    ;; :aa was read but is not mediated by a cell
    ;; and in Jellz we take those as declarations
    ;; that they will not change over the life of
    ;; the model instance so do not record them
    ;; as used.
    ;;
    (is (= #{:bb} (slot-useds wow :cc)))

    (jz-set! wow :bb 5)
    (is (= 5 (bb wow)))
    (is (= 11 (cc wow)))    

    (is (= (cc wow) @cco))))

;; --- no cycles allowed for now -----------------------
;;

(deftest cyclic-dependency-detection
  ;; 
  ;; aa is a function of cc which is a function of aa
  ;;
  ;; not good, but let's throw an exception, not
  ;; loop infinitely
  ;;
  (is (thrown-with-msg? 
       java.lang.Exception
       #"cyclic dependency"
       (let [wow  (mkjzo ::thingy
                         :aa (jzf (+ 42 (cc me)))
                         :bb (jzi 3)
                         :cc (jzf (+ (aa me)
                                     (* 2 (bb me))))
                         )]
         (assert (= 1 (aa wow)))
         (assert (= 3 (bb wow)))
         (assert (= 7 (cc wow)))))))

(defmd ::thing-1 [])

(def b2b-a (atom nil))

(api/defobserver :b2b [::thing-1][]
  (reset! b2b-a new-val))

(def-jzo-slots b2b dd ee)

#_
(ns-unmap *ns* 'test-cyclic-dependency-msg)

(deftest test-data-push
  (let [wow  (mkjzo ::thing-1
                    :aa (jzi 1)
                    :b2b (jzf (* 3 (aa me))))]
    (is (= 1 (aa wow)))
    (is (= 3 (b2b wow)))
    (is (= @b2b-a 3))
    (is (= 1 (count (:users @(:aa @wow)))))

    (jz-set! wow :aa 2) ;; bam! we kick off a cascade of change

    ; make sure flow happened eagerly by looking if
    ; the observer fired and repopulated our atom before
    ; we read a dependent variable (which itself would force
    ; necessary recalculation, but we want the flow to happen
    ; unasked):
    ;
    (is (= @b2b-a 6))
    ;
    ;... and now we consult the value just to be sure
    ;
    (is (= 6 (b2b wow)))))


;;; --- The Pentagram of Death: a hard use case for data integrity ------

(defmd ::pod [])
(def-jzo-slots a7 a70)

#_
(ns-unmap *ns* 'pentagram-of-death)

#_
(clojure.test/test-vars [#'pentagram-of-death])

#_
(alter-var-root #'*trx?* not)

(deftest pentagram-of-death
  ;
  ; Christened the Pentagram of Death by Phillip J Eby, this
  ; is the use case that challenges an engine not to calculate
  ; and observe transiently* inconsistent values when two different
  ; dependency paths of one slot (here :ee) lead back to 
  ; the same slot (:aa).
  ;
  ; * "Transiently" because the state change propagation eventually*
  ;   gets :ee to the value consistent with the new state (* which is not
  ;   good enough because observers may have already fired and produced
  ;   side effects off the invalid state.
  ;
  ; The example is contrived but was contrived to replicate
  ; a real dataflow failure that arose in my RoboCup simulation and 
  ; prompted Cells 3 and the concept of data integrity.
  ;
  ; For the telling story behind the useless slot names :aa, :bb et al
  ; please see: http://smuglispweeny.blogspot.com/2008/07/aa-bb-cc-and-dd.html
  ;
   (let [run (atom {})
         obs (atom {})

         rset (fn []
                (swap! run empty)
                (swap! obs empty))

         logit (fn [log key]
                  (swap! run assoc key
                         (inc (key @run 0))))
         logrun #(logit run %)


         verify-p-current (fn [p]
                          (is (= 2 (aa p)))
                          (is (= 2 (bb p)))
                          (is (= 20 (cc p)))
                          (is (= 200 (dd p)))
                          (is (= 2000072 (ee p))))

         podobs (fn [slot me new old]
                  (swap! obs assoc slot
                         (inc (slot @obs 0))))

         p (mkjzo ::pod
                  :aa (jzi+ {:obs podobs} 1)
                  :a7 (jzi+ {:obs podobs} 7)
                  :a70 (jzf+ {:obs podobs}
                             (logrun :a70)
                             (* 10 (a7 me)))
                  :bb (jzf+ {:obs podobs}
                            (logrun :bb)
                            (aa me))
                  :cc (jzf+ {:obs podobs}
                            (logrun :cc)
                            (* 10 (aa me)))
                  :dd (jzf+ {:obs podobs}
                            (logrun :dd)
                            (if (even? (bb me))
                              (* 10 (cc me))
                              42))
                  :ee (jzf+ {:obs podobs}
                            (logrun :ee)
                            (+ (a70 me) (bb me) (* 10000 (dd me))))
                  )]
     
     ;; next checks are just that the engine calculated well
     ;; and built a good dependency graph
     ;;
      (is (= 1 (aa p)))
      (is (= 1 (bb p)))
      (is (= 10 (cc p)))
      (is (= 42 (dd p)))
      (is (= 420071 (ee p)))

      (is (= #{} (slot-useds p :aa)))
      (is (= #{:bb :cc} (slot-users p :aa)))
      
      (is (= #{:aa} (slot-useds p :bb)))
      (is (= #{:dd :ee} (slot-users p :bb)))
      
      (is (= #{:aa} (slot-useds p :cc)))
      (is (= #{} (slot-users p :cc)))
      
      (is (= #{:bb} (slot-useds p :dd)))
      (is (= #{:ee} (slot-users p :dd)))
      
      (is (= #{:a70 :bb :dd} (slot-useds p :ee)))
      (is (= #{} (slot-users p :ee)))
      
      ;; now we come to data integrity: when change happens
      ;; do all and only those cells affected recalculate
      ;; and reobserve and do so exactly once.
      ;;
      (binding [*trx?* true]
        (rset)

        (jz-set! p :aa (inc (aa p)))

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
          (is (and (keyword? k)
                   (= 1 v))))
   
        ; check that this time dd branched to use cc as well as bb
        ;
        (is (= #{:bb :cc} (slot-useds p :dd)))
        
        (verify-p-current p)
        
        (jz-set! p :aa (inc (aa p)))

        ; :aa hence :bb now odd so :dd goes back to 42
        ;
        (is (= 42 (dd p)))
        ;
        ; ...and check dependency on :cc got pruned
        ;
        (is (= #{:bb} (slot-useds p :dd)))
        )))

