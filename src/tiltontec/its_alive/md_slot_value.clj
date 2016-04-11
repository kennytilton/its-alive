(ns tiltontec.its-alive.md-slot-value
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))


(defn md-slot-value [self slot-name]
  (when-not (mdead? self)
    ;; we  used to go kersplat in here but a problem got created just to
    ;; get its solution for a hint after which the whole shebang could
    ;; be not-to-be'd but still be interrogated for the hinting purposes
    ;; so... why not? The dead can still be useful.
    ;; If the messages get annoying do something explicit to say
    ;; not-to-be-but-expecting-use
    (if-let [c (md-slot-cell self slot-name)]
      (cell-read c)
      (slot-value self slot-name))))

  
(def ^:dynamic *trc-ensure* nil)

(defn qci [c]
  (when c
    (cons (md-name (c-model c)) (c-slot-name c))))

;; ;-------------------------------------------------------------

;; (defn md-slot-makunbound (self slot-name
;;                             &aux (c (md-slot-cell self slot-name)))
;;   (unless c
;;     (c-break ":md-slot-makunbound > cellular slot ~a of ~a cannot be unbound unless initialized as input?"
;;       slot-name self))
  
;;   (when (c-unboundp c)
;;     (return-from md-slot-makunbound nil))

;;   (when *within-integrity* ;; 2006-02 oops, bad name
;;     (c-break "md-slot-makunbound of ~a must be deferred by wrapping code in with-integrity" c))
  
;;   ; 
;;   ; Big change here for Cells III: before, only the propagation was deferred. Man that seems
;;   ; wrong. So now the full makunbound processing gets deferred. Less controversially,
;;   ; by contrast the without-c-dependency wrapped everything, and while that is harmless,
;;   ; it is also unnecessary and could confuse people trying to follow the logic.
;;   ;
;;   (let ((causation *causation*))
;;     (with-integrity (:change c)
;;       (let ((*causation* causation))
;;         ; --- cell & slot maintenance ---
;;         (let ((prior-value (c-value c)))
;;           (setf (c-value-state c) :unbound
;;             (c-value c) nil
;;             (c-state c) :awake)
;;           (bd-slot-makunbound self slot-name)
;;           ;
;;            ; --- data flow propagation -----------
;;           ;
;;           (without-c-dependency
;;               (c-propagate c prior-value t)))))))

;;; --- setf md.slot.value --------------------------------------------------------
;;;

(defn md-slot-value-reset! [self slot-name new-value]
  (let [c (md-slot-cell self slot-name)]
    (cond
     (nil? c)
     (err format "Non-c-in slot ~a of ~a cannot be reset! to ~s"
          slot-name self new-value)
     ;; ---------------------------------------------------
     (cl-find (c-lazy c) [:once-asked :always true])
     (md-slot-value-assume c new-value nil) ;; I can see :no-pragate here eventually
     ;; ------------------------------
     *defer-changes*
      (err format "SETF of ~a must be deferred by wrapping code in WITH-INTEGRITY ~a" c *within-integrity*)
   
     :else (with-integrity (:change slot-name)
               (md-slot-value-assume c new-value nil)))))

                    
(defn md-slot-value-assume [c raw-value propagation-code]
  (assert c)
  (without-c-dependency
   (let [prior-value (c-value c)
         absorbed-value (c-absorb-value c raw-value)]

        (c-pulse+-update c :slotv-assume)

        (when-not
            ;; -- bail if unchanged -----------------------
            (and (not (eq propagation-code :propagate))
                 (c-no-news c absorbed-value prior-value))
          ;; 
          ;; --- slot maintenance ---
        
          (unless (c-synaptic c) 
                  (md-slot-value-store (c-model c) (c-slot-name c) absorbed-value))
        
          ;; --- cell maintenance ---
          (ref-setf (:value c) absorbed-value)
          (ref-setf (:state c) :awake)
        
          (let [callers (c-callers c)]
            (when-let [optimize (and (typep c 'c-dependent)
                                     (c-optimize c))]
              (case optimize
                :when-value-t (when (c-value c)
                                (c-unlink-from-used c))
                true (c-optimize-away?! c))) ;; so coming propagation has it visible
        
            ;; --- data flow propagation -----------
            (unless (eq propagation-code :no-propagate)
                    (c-propagate c prior-value callers))) 

          absorbed-value))))

;---------- optimizing away cells whose dependents all turn out to be constant ----------------
;

(defn c-optimize-away?! [c]
  (when (and (typep c 'c-dependent)
             (nil? (cd-useds c))
             (cd-optimize c)
             (not (c-optimized-away? c)) ;; c-streams (FNYI) may come this way repeatedly even if optimized away
             (c-valid? c) ;; /// when would this not be the case? and who cares?
             (not (c-synaptic c)) ;; no slot to cache invariant result, so they have to stay around)
             (not (c-input? c)) ;; yes, dependent cells can be inputp
             )
    (ref-setf (:state c) :optimized-away)
    (when-let [me (c-model c)]
      (ref-set (:cells me) (dissoc (:cells @me) (c-slot c))))
    (md-cell-flush c)
    
    ;; let callers know they need not check us for currency again
    (doseq [caller (seq (c-callers c))]
      ;
      ; example: on window shutdown with a tool-tip displayed, the tool-tip generator got
      ; kicked off and asked about the value of a dead instance. That returns nil, and
      ; there was no other dependency, so the Cell then decided to optimize itself away.
      ; of course, before that time it had a normal value on which other things depended,
      ; so we ended up here. where there used to be a break.
      ;
      (alter caller assoc :useds (remove #{c} (cd-useds caller)))
      (caller-drop c caller)
      ;;; (trc "nested opti" c caller)
      (c-optimize-away?! caller) ;; rare but it happens when rule says (or .cache ...)
      )))

(export! dump-call-stack)    
(defn dump-call-stack [dbgid &optional dbgdata]
  (trx dump-call-stack-newest-first (length *call-stack*) dbgid dbgdata)
  (doseq [caller *call-stack*]
    (trx "caller> " caller #+shhh (cr-code caller))))
