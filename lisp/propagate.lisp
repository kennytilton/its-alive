﻿;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt



|#

(in-package :cells) 

;----------------- change detection ---------------------------------

(defun c-no-news (c new-value old-value)
  ;;; (trc nil "c-no-news > checking news between" newvalue oldvalue)
  (bif (test (c-unchanged-test (c-model c) (c-slot-name c)))
      (funcall test new-value old-value)
      (eql new-value old-value)))

(defmacro def-c-unchanged-test ((class slotname) &body test)
  `(defmethod c-unchanged-test ((self ,class) (slotname (eql ',slotname)))
     ,@test))
     
(defmethod c-unchanged-test (self slotname)
  (declare (ignore self slotname))
  nil)

; --- data pulse (change ID) management -------------------------------------

(defparameter *one-pulse?* nil)

(defparameter *data-pulse-lock* (mp:make-process-lock))
(defparameter *dp-log* nil)


(defun data-pulse-next (pulse-info)
  (declare (ignorable pulse-info))
  (unless *one-pulse?*
    (mp:with-process-lock (*data-pulse-lock*)
      #+xxxxx (when *dp-log*
        (trc "dp-next> " (1+ *data-pulse-id*) pulse-info))
      #+chill (when *c-debug*
                (push (list :data-pulse-next pulse-info) *istack*))
      (incf *data-pulse-id*))))

(defun c-currentp (c)
  (eql (c-pulse c) *data-pulse-id*))

(defun c-pulse-update (c key)
  (declare (ignorable key))
  #+hunh?
  (unless (find key '(:valid-uninfluenced))
    (trc nil "!!!!!!! c-pulse-update updating !!!!!!!!!!" *data-pulse-id* c key :prior-pulse (c-pulse c)))
  (unless (>= *data-pulse-id* (c-pulse c))
    (describe c))
  (assert (>= *data-pulse-id* (c-pulse c)) ()
    "Current DP ~a not GE pulse ~a of cell ~a" *data-pulse-id* (c-pulse c) c)
  (setf (c-pulse c) *data-pulse-id*))

;--------------- propagate  ----------------------------
; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.

(defparameter *per-cell-handler* nil)

(defun c-propagate (c prior-value prior-value-supplied &optional (callers (c-callers c)))
  (when *one-pulse?*
    (when *per-cell-handler*
      (funcall *per-cell-handler* c prior-value prior-value-supplied)
      (return-from c-propagate)))

  (count-it :cpropagate)
  (setf (c-pulse-last-changed c) *data-pulse-id*)
          
  (let (*depender* *call-stack* ;; I think both need clearing, cuz we are neither depending nor calling when we prop to callers
        (*c-prop-depth*  (1+ *c-prop-depth*))
        (*defer-changes* t))
    (trc nil "c.propagate clearing *depender*" c)
    
    ;------ debug stuff ---------
    ;
    (when *stop*
      (princ #\.)(princ #\!)
      (return-from c-propagate))    
    (trc nil  "c.propagate> !!!!!!! propping" c (c-value c) :caller-ct (length (c-callers c)))
    #+slow (trc nil "c.propagate> !!!! new value" (c-value c) :prior-value prior-value :caller-ct (length (c-callers c)) c)
    (when *c-debug*
      (when (> *c-prop-depth* 250)
        (trc nil "c.propagate deep" *c-prop-depth* (c-model c) (c-slot-name c) #+nah c))
      (when (> *c-prop-depth* 300)
        (c-break "c.propagate looping ~c" c)))
    
    ; --- manifest new value as needed ---
    ;
    ; 20061030 Trying not.to.be first because doomed instances may be interested in callers
    ; who will decide to propagate. If a family instance kids slot is changing, a doomed kid
    ; will be out of the kids but not yet quiesced. If the propagation to this rule asks the kid
    ; to look at its siblings (say a view instance being deleted from a stack who looks to the psib
    ; pb to decide its own pt), the doomed kid will still have a parent but not be in its kids slot
    ; when it goes looking for a sibling relative to its position.
    ;
    (when (and prior-value
            (md-slot-owning? (type-of (c-model c)) (c-slot-name c)))
      (trc nil "c.propagate> contemplating lost" (qci c))
      (b-when lost (#-bammd set-difference #+bammd stable-set-difference (list! prior-value) (list! (c-value c)))
        (trc nil "prop nailing owned!!!!!!!!!!!" (qci c) :lost (length lost))
        (map nil 'not-to-be lost)))
    
    ; propagation to callers jumps back in front of client slot--value-observe handling in cells3
    ; because model adopting (once done by the kids change handler) can now be done in
    ; shared-initialize (since one is now forced to supply the parent to make-instance).
    ;
    ; we wnat it here to support (eventually) state change rollback. change handlers are
    ; expected to have side-effects, so we want to propagate fully and be sure no rule
    ; wants a rollback before starting with the side effects.
    ; 
    (progn ;; unless (member (c-lazy c) '(t :always :once-asked)) ;; 2006-09-26 still fuzzy on this 
      (c-propagate-to-callers c callers))
    
    (trc nil "c.propagate observing" c)


    (when (or (> *data-pulse-id* (c-pulse-observed c))
            (find (c-lazy c) '(:once-asked :always t))) ;; messy: these can get setfed/propagated twice in one pulse
      
      ;;;  check this with its-alive off and see if we should check here for pulse already done
      ;;(trc nil ":propagate-pulsing" :*dpid* *data-pulse-id* :cdpid (c-pulse-observed c) c)
      (b-if flushed (md-slot-cell-flushed (c-model c) (c-slot-name c))
        (setf (flushed-cell-pulse-observed flushed) *data-pulse-id*)
        (setf (c-pulse-observed c) *data-pulse-id*))
      (let ((*observe-why* :propagate))
        (slot-value-observe (c-slot-name c) (c-model c)
          (c-value c) prior-value prior-value-supplied c)))
    
    
    ;
    ; with propagation done, ephemerals can be reset. we also do this in c-awaken, so
    ; let the fn decide if C really is ephemeral. Note that it might be possible to leave
    ; this out and use the datapulse to identify obsolete ephemerals and clear them
    ; when read. That would avoid ever making again bug I had in which I had the reset inside slot--value-observe,
    ; thinking that that always followed propagation to callers. It would also make
    ; debugging easier in that I could find the last ephemeral value in the inspector.
    ; would this be bad for persistent CLOS, in which a DB would think there was still a link
    ; between two records until the value actually got cleared?
    ;
    (ephemeral-reset c)))

; --- slot change -----------------------------------------------------------

(defmacro defobserver (slotname &rest args &aux (aroundp (eq :around (first args))))
  (when aroundp (setf args (cdr args)))

  #+irritating
  (when (find slotname '(value kids))
    (warn "d: did you mean .value or .kids when you coded ~a?" slotname))

  (destructuring-bind ((&optional (self-arg 'self) (new-varg 'new-value)
                         (oldvarg 'old-value) (oldvargboundp 'old-value-boundp) (cell-arg 'c))
                       &body output-body) args
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',slotname :output-defined) t))
       ,(if (eql (last1 output-body) :test)
            (let ((temp1 (gensym))
                  (loc-self (gensym)))
              `(defmethod slot-value-observe #-(or cormanlisp) ,(if aroundp :around 'progn)
                 ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp ,cell-arg)
                 (let ((,temp1 (bump-output-count ,slotname))
                       (,loc-self ,(if (listp self-arg)
                                       (car self-arg)
                                     self-arg)))
                   (when (and ,oldvargboundp ,oldvarg)
                     (format t "~&output ~d (~a ~a) old: ~a" ,temp1 ',slotname ,loc-self ,oldvarg ,cell-arg))
                   (format t "~&output ~d (~a ~a) new: ~a" ,temp1 ',slotname ,loc-self ,new-varg ,cell-arg))))
          `(defmethod slot-value-observe
               #-(or cormanlisp) ,(if aroundp :around 'progn)
             ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp ,cell-arg)
             (declare (ignorable
                       ,@(flet ((arg-name (arg-spec)
                                  (etypecase arg-spec
                                    (list (car arg-spec))
                                    (atom arg-spec))))
                           (list (arg-name self-arg)(arg-name new-varg)
                             (arg-name oldvarg)(arg-name oldvargboundp) (arg-name cell-arg)))))
             ,@output-body)))))

(defmacro dbgobserver (slotname &optional params &body body)
  `(defobserver ,slotname ,params
     (trcx ,(conc$ 'obs- slotname) self (md-name self) new-value old-value old-value-boundp c)
     ,@body))

(export! dbgobserver)

(defmacro bump-output-count (slotname) ;; pure test func
  `(if (get ',slotname :outputs)
       (incf (get ',slotname :outputs))
     (setf (get ',slotname :outputs) 1)))

; --- recalculate dependents ----------------------------------------------------


(defmacro cll-outer (val &body body)
 `(let ((outer-val ,val))
    ,@body))

(defmacro cll-inner (expr)
  `(,expr outer-val))

(export! cll-outer cll-inner)


(defun c-propagate-to-callers (c callers)
  ;
  ;  We must defer propagation to callers because of an edge case in which:
  ;    - X tells A to recalculate
  ;    - A asks B for its current value
  ;    - B must recalculate because it too uses X
  ;    - if B propagates to its callers after recalculating instead of deferring it
  ;       - B might tell H to reclaculate, where H decides this time to use A
  ;       - but A is in the midst of recalculating, and cannot complete until B returns.
  ;         but B is busy eagerly propagating. "This time" is important because it means
  ;         there is no way one can reliably be sure H will not ask for A
  ;
  (when (find-if-not (lambda (caller)
                       (and (c-lazy caller) ;; slight optimization
                         (member (c-lazy caller) '(t :always :once-asked))))
          callers)
    (let ((causation (cons c *causation*))) ;; in case deferred
      #+slow (trc nil "c.propagate-to-callers > queueing notifying callers" callers)
      (with-integrity (:tell-dependents c)
        (assert (null *call-stack*))
        (assert (null *depender*))
        ;
        (if (mdead (c-model c))
            (trc nil "WHOAA!!!! dead by time :tell-deps dispatched; bailing" c)
          (let ((*causation* causation))
            (trc nil "c.propagate-to-callers > actually notifying callers of" c callers)
            #+c-debug (dolist (caller callers)
                        (assert (find c (cd-useds caller)) () "test 1 failed ~a ~a" c caller))
            #+c-debug (dolist (caller (copy-list callers)) ;; following code may modify c-callers list...
                        (trc nil "PRE-prop-CHECK " c :caller caller (c-state caller) (c-lazy caller))
                        (unless (or (eq (c-state caller) :quiesced) ;; ..so watch for quiesced
                                  (member (c-lazy caller) '(t :always :once-asked)))
                          (assert (find c (cd-useds caller))() "Precheck Caller ~a of ~a does not have it as used" caller c)
                          ))
            (dolist (caller callers)
              (trc nil #+chill *c-prop-dep-trace* "propagating to caller iterates" c :caller caller (c-state caller) (c-lazy caller))
              (block do-a-caller
                (unless (or (eq (c-state caller) :quiesced) ;; ..so watch for quiesced
                          (member (c-lazy caller) '(t :always :once-asked)))
                  (unless (find c (cd-useds caller))
                    (unless (c-optimized-away-p c) ;; c would have been removed from any c-useds
                      #+shhh (progn
                               (describe caller)
                               (describe (c-model caller))
                               (describe c))
                      (cond 
                       ((find caller (c-callers c))
                        (trc "WHOA!!!! Bailing on Known caller:" caller :w/out-this-callee-in-its-used c))
                       (t (trc nil "Bailing on caller lost during propagation:" caller :w/out-this-callee-in-its-used c)))
                      (return-from do-a-caller)))
                  #+slow (trc nil "propagating to caller is used" c :caller caller (c-currentp c))
                  (let ((*trc-ensure* (trcp c)))
                    ;
                    ; we just calculate-and-set at the first level of dependency because
                    ; we do not need to check the next level (as ensure-value-is-current does)
                    ; because we already know /this/ notifying dependency has changed, so yeah,
                    ; any first-level cell /has to/ recalculate. (As for ensuring other dependents
                    ; of the first level guy are current, that happens automatically anyway JIT on
                    ; any read.) This is a minor efficiency enhancement since ensure-value-is-current would
                    ; very quickly decide it has to re-run, but maybe it makes the logic clearer.
                    ;
                    ;(ensure-value-is-current caller :prop-from c) <- next was this, but see above change reason
                    ;
                    (unless (c-currentp caller) ; happens if I changed when caller used me in current pulse
                      (calculate-and-set caller :propagate c))))))))))))

(defparameter *the-unpropagated* nil)

(defmacro with-one-datapulse ((&key (per-cell nil per-cell?) (finally nil finally?)) &body body)
  `(call-with-one-datapulse (lambda () ,@body)
     ,@(when per-cell? `(:per-cell (lambda (c prior-value prior-value-boundp)
                                     (declare (ignorable c prior-value prior-value-boundp))
                                     ,per-cell)))
     ,@(when finally? `(:finally (lambda (cs) (declare (ignorable cs)) ,finally)))))

(defun call-with-one-datapulse
    (f &key
      (per-cell (lambda (c prior-value prior-value?)
                  (unless (find c *the-unpropagated* :key 'car)
                    (pushnew (list c prior-value prior-value?) *the-unpropagated*))))
      (finally (lambda (cs)
                 (print `(finally sees ,*data-pulse-id* ,cs))
                 (loop for (c prior-value prior-value?) in (nreverse cs) do
                       (c-propagate c prior-value prior-value?)))))
  (assert (not *one-pulse?*))
  (data-pulse-next :client-prop)
  (trc "call-with-one-datapulse bumps pulse" *data-pulse-id*)
  (funcall finally
    (let ((*one-pulse?* t)
          (*per-cell-handler* per-cell)
          (*the-unpropagated* nil))
      (funcall f)
      *the-unpropagated*)))
  
