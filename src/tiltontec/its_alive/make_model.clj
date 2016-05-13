(ns tiltontec.its-alive.make-model
  (:require
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   [tiltontec.its-alive.evaluate :refer :all]
   [tiltontec.its-alive.integrity :refer :all]
   [tiltontec.its-alive.observer :refer :all]
   ))

(def-rmap-slots md-
  name)

(def-rmap-meta-slots md-
  state cz)

;;; --- accessors ----

(defn md-get [me slot]
  (trx :md-get!!!!!!! slot (md-name me))
  (if-let [c  (slot (:cz (meta me)))]
    (c-get c)
    (:slot @me)))

(defn md-reset! [me slot new-value]
  (trx :md-reset!!!!!!! slot (md-name me) new-value)
  (if-let [c  (slot (:cz (meta me)))]
    (c-reset! c new-value)
    (do
      (err format "change to slot %s not mediated by cell" slot)
      (rmap-setf [slot me] new-value))))

;;; --- md initialization ---

(declare md-awaken)

(defn md-install-cell [me slot c]
  ;; note that c (a misnomer) might not be a Cell
  (cond
    (c-ref? c) (do
                 (alter c assoc
                        :slot slot
                        :me me)
                 (rmap-setf [slot me]
                            (when (c-input? c)
                              (c-value c)))
                 true)
    :else (do
            (rmap-setf [slot me] c)
            false)))

(defn make [& iargs]
  (dosync
   (let [me (ref (->> iargs
                      (partition 2)
                      (map (fn [[k v]]
                             (vector k (if (c-ref? v)
                                         unbound
                                         v))))
                      (into {}))
                 :meta {:state :nascent})]
     (assert (meta me))
     (rmap-meta-setf
      [:cz me]
      (->> iargs
           (partition 2)
           (filter (fn [[slot v]]
                     (md-install-cell me slot v)))
           (map vec)
           (into {})))
     (with-integrity (:awaken me)
       (md-awaken me)))))
  
(defn md-awaken
  "(1) do initial evaluation of all ruled slots
   (2) call observers of all slots"
  [me]

  (c-assert (= :nascent (md-state me)))
  (rmap-meta-setf [:state me] :awakening)
  (trx :md-awk @me)
  (doall
   (for [slot (keys @me)]
     (do (trx :md-awk-slot slot)
         (if-let [c (slot (md-cz me))]
           (c-awaken c)
           (observe slot me (slot @me) unbound nil)))))

  (rmap-meta-setf [:state me] :awake)
  me)

;;; --- utilities, accessors, etc --------------------------------------

;; (defmethod c-slot-value ((me model-object) slot)
;;   (slot-value me slot))

;; (defmethod md-slot-cell (me slot-name)
;;   (if me
;;       (cdr (assoc slot-name (cells me)))
;;     (get slot-name 'cell)))

;; (defn md-cell-flush [c]
;;   (push (cons (c-slot-name c)
;;           #+gogo (c-pulse+-observed c)
;;           #-gogo c)
;;     (cells-flushed (c-model c))))

;; (defn md-slot-cell-flushed (me slot-name)
;;   (if me
;;       (assoc slot-name (cells-flushed me))
;;     (get slot-name 'cell)))

;; (defn flushed-cell-pulse-observed [c]
;;   (if (numberp (cdr c)) (cdr c) (c-pulse-observed (cdr c))))

;; (defn (setf flushed-cell-pulse+-observed) (pulse c)
;;   (if (numberp (cdr c))
;;       (rplacd c pulse+)
;;     (progn
;;       ;; (trx "flush-pulsing" :new pulse+ :old (if (numberp (cdr c)) (cdr c) (c-pulse-observed (cdr c)))(c-slot-name c))
;;       (setf (c-pulse+-observed (cdr c)) pulse))))

;; #+test
;; (get 'cgtk::label :cell-types)

;; (defn md-slot-cell-type (class-name slot-name)
;;   (assert class-name)
;;   (if (eq class-name 'null)
;;       (get slot-name :cell-type)
;;     (bif (entry (assoc slot-name (get class-name :cell-types)))
;;       (cdr entry)
;;       (dolist (super (class-precedence-list (find-class class-name))
;;                 (setf (md-slot-cell-type class-name slot-name) nil))
;;         (bwhen (entry (assoc slot-name (get (c-class-name super) :cell-types)))
;;           (return-from md-slot-cell-type
;;             (setf (md-slot-cell-type class-name slot-name) (cdr entry))))))))

;; (defn (setf md-slot-cell-type) (new-type class-name slot-name)
;;   (assert class-name)
;;   (if (eq class-name 'null) ;; not def-c-variable
;;       (setf (get slot-name :cell-type) new-type)
;;     (let ((entry (assoc slot-name (get class-name :cell-types))))
;;       (if entry
;;           (prog1
;;             (setf (cdr entry) new-type)
;;             (loop for c in (class-direct-subclasses (find-class class-name))
;;                 do (setf (md-slot-cell-type (class-name c) slot-name) new-type)))
;;         (cdar (push (cons slot-name new-type) (get class-name :cell-types)))))))



;; (defn (setf md-slot-owning-direct?) (value class-name slot-name)
;;   (assert class-name)
;;   (if (eq class-name 'null) ;; global variables
;;       (setf (get slot-name :owning) value)
;;     (progn
;;       (bif (entry (assoc slot-name (get class-name :direct-ownings)))
;;         (setf (cdr entry) value)
;;         (push (cons slot-name value) (get class-name :direct-ownings)))
;;       ; -- propagate to derivatives ...
;;       (labels ((clear-subclass-ownings [c]
;;                  (loop for sub-c in (class-direct-subclasses c)
;;                      for sub-c-name = (c-class-name sub-c)
;;                      do (setf (get sub-c-name :indirect-ownings)
;;                           (delete slot-name (get sub-c-name :indirect-ownings) :key 'car)) ;; forces redecide
;;                        (setf (get sub-c-name :model-ownings) nil) ;; too much forcing full recalc like this?
;;                        (clear-subclass-ownings sub-c))))
;;         (clear-subclass-ownings (find-class class-name))))))

;; (defn md-owning-slots (me &aux (st (type-of me)))
;;   (or (get st :model-ownings)
;;     (setf (get st :model-ownings)
;;       (loop for s in (class-slots (class-of me))
;;           for sn = (slot-definition-name s)
;;           when (and (md-slot-cell-type st sn)
;;                  (md-slot-owning? st sn))
;;           collect sn))))

;; (defn md-slot-value-store (me slot-name new-value)
;;   (trx nil "md-slot-value-store" me slot-name new-value)
;;   (if me
;;     (setf (slot-value me slot-name) new-value)
;;     (setf (symbol-value slot-name) new-value)))

;----------------- navigation: slot <> initarg <> esd <> cell -----------------

;; (defn (setf md-slot-cell) (new-cell me slot-name)
;;   (if me ;; not on def-c-variables
;;       (bif (entry (assoc slot-name (cells me)))
;;         ; this next branch guessed it would only occur during kid-slotting,
;;         ; before any dependency-ing could have happened, but a math-editor
;;         ; is silently switching between implied-multiplication and mixed numbers
;;         ; while they type and it 
;;         (progn
;;           (trx nil "second cell same slot:" slot-name :old entry :new new-cell)
;;           (let ((old (cdr entry))) ;; s/b being supplanted by kid-slotter
;;             (declare (ignorable old))
;;             (c-assert (null (c-callers old)))
;;             (when (typep entry 'c-dependent)
;;               (c-assert (null (cd-useds old))))
;;             (trx nil "replacing in model .cells" old new-cell me)
;;             (rplacd entry new-cell)))
;;         (progn
;;           (trx nil "adding to model .cells" new-cell me)
;;           (push (cons slot-name new-cell)
;;             (cells me))))
;;     (setf (get slot-name 'cell) new-cell)))

;; (defn md-map-cells (me type celldo)
;;   (map type (fn (cell-entry)
;;                 (bwhen (cell (cdr cell-entry))
;;                        (unless (listp cell)
;;                          (celldo cell))))
;;         (cells me)))
