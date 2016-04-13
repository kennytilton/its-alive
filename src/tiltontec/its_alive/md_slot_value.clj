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
;;               (propagate c prior-value (c-callers c))))))))

;;; --- setf md.slot.value --------------------------------------------------------
;;;

(defn md-slot-value-reset! [self slot-name new-value]
  (let [c (md-slot-cell self slot-name)]
    (cond
     (nil? c)
     (err format "Non-c-in slot ~a of ~a cannot be reset! to ~s"
          slot-name self new-value)
     ;; ---------------------------------------------------
     (some #{(c-lazy c)} [:once-asked :always true])
     (slot-value-assume c new-value nil) ;; I can see :no-pragate here eventually
     ;; ------------------------------
     *defer-changes*
      (err format "SETF of ~a must be deferred by wrapping code in WITH-INTEGRITY ~a" c *within-integrity*)
   
     :else (with-integrity (:change slot-name)
               (slot-value-assume c new-value nil)))))


