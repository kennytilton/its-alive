(comment

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton



)


(eval-when (compile eval load)
  (export '(c-envalue)))

(comment ;; defstruct (c-envaluer (:conc-name nil))
  envalue-rule)

(defmethod awaken-cell [c]
  (declare (ignorable c)))

(defmethod awaken-cell ((c cell))
  (assert (c-input? c))
  ;
  ; nothing to calculate, but every cellular slot should be output
  ;
  (when (> @+pulse+ (c-pulse-observed c))
    ;(trx nil "awaken-pulsing" :*dpid* @+pulse+ :cdpid (c-pulse-observed c) c)
    (setf (c-pulse+-observed c) @+pulse+)
    (trx nil "awaken cell observing" c @+pulse+)
    (let ((*observe-why* :awaken-cell))
      (observe (c-slot-name c) (c-model c) (c-value c) nil nil c))
    (ephemeral-reset c)))

(defmethod awaken-cell ((c c-ruled))
  (let (*depender*)
    (calculate-and-set c :fn-awaken-cell nil)))

#+cormanlisp ; satisfy CormanCL bug
(defmethod awaken-cell ((c c-dependent))
  (let (*depender*)
    (trx nil "awaken-cell c-dependent clearing *depender*" c)
    (calculate-and-set c :fn-awaken-cell nil)))

(defmethod awaken-cell ((c c-drifter))
  ;
  ; drifters *begin* valid, so the derived version's test for unbounditude
  ; would keep (drift) rule ever from being evaluated. correct solution
  ; (for another day) is to separate awakening (ie, linking to independent
  ; cs) from evaluation, tho also evaluating if necessary during
  ; awakening, because awakening's other role is to get an instance up to speed
  ; at once upon instantiation 
  ;
  (calculate-and-set c :fn-awaken-cell nil)
  (cond ((c-valid? c) (c-value c))
        ((c-unbound? c) nil)
        (t "illegal state!!!")))
