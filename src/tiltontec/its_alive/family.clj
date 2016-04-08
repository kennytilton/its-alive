(comment

    Cells -- Automatic Dataflow Managememnt



)


(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(model value family dbg .pa par
             kids kid1 .kid1 kid2 .kid2 last-kid ^k-last perishable)))

(defmodel model ()
  ((.md-name :cell nil :initform nil :initarg :md-name :accessor md-name)
   (.fm-parent :cell nil :initform nil :initarg :fm-parent :accessor fm-parent)
   ;;(.dbg-par :cell nil :initform nil)
   (.value :initform nil :accessor value :initarg :value)
   (register? :cell nil :initform nil :initarg :register? :reader register?)
   (zdbg :initform nil :accessor dbg :initarg :dbg)))



(defmethod md-awaken :around ((self model))
  (when (eq :eternal-rest (md-state .pa))
    (trcx not-awakening-child-of-dead!!!!!!!!!! self .pa)
    (setf (md-state self) :eternal-rest)
    (return-from md-awaken self))
  (call-next-method)
  self)

(defn zwibble ()
  (error "value invoked on nil"))

(defmethod value ((nada null))
  ;; looking for tighter backtrace than method-not-found
  (zwibble))

(defmethod md-finalize ((self model))
  ;;.bgo
  (unless (or (slot-value self '.fnz)
            (slot-value self '.doomed)
            (eq (slot-value self '.md-state) :eternal-rest))
    ;(print `(:fz-not-to-be!!! ,(slot-value self '.md-name) ,(type-of self)))
    (setf (slot-value self '.fnz) t)
    (not-to-be self)))

(defmethod initialize-instance :after ((self model) &key)
  ;(excl:schedule-finalization self 'md-finalize)
  (when (register? self)
    (fm-register self)))

(defmethod print-cell-object ((md model))
  (or (md-name md) :md?))

(defmethod fm-parent (other)
  (declare (ignore other))
  nil)

(defmethod (setf fm-parent) (new-value other)
  (declare (ignore other))
  new-value)

(defmethod print-object ((self model) s)
  #+shhh (format s "~a" (type-of self))
  (format s "~a~a" (if (mdead self) "DEAD!" "")
    (or (md-name self) (type-of self))))

(defmethod md-name (other)
  (trc "yep other md-name" other (type-of other))
  other)

(defmethod md-name ((nada null))
  (unless (c-stopped)
    (error "md-name called on nil")
    (c-stop :md-name-on-null)
    (brk "md-name called on nil")))

(defmethod md-name ((sym symbol)) sym)

(defmethod shared-initialize :around ((self model) slotnames & initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))

  (call-next-method)

  (when (slot-boundp self '.md-name)
    (unless (md-name self)
      ; lotsa symbols over time in a web app
      ;(setf (md-name self) (gentemp (string (c-class-name (class-of self)))))
      (setf (md-name self) #+live (type-of self)
        #-live (gensym (string (c-class-name (class-of self)))))
      ))
 
  (when (and (slot-boundp self '.fm-parent)
          (fm-parent self)
          (zerop (adopt-ct self)))
      (md-be-adopted self)))

(defmodel perishable ()
  ((expiration :initform nil :accessor expiration :initarg :expiration)))

(defobserver expiration ()
  (when new-value
    (not-to-be self)))

(def ^:dynamic *parent* nil)

(defmodel family (model)
  ((.kid-slots :cell nil
     :initform nil
     :accessor kid-slots
     :initarg :kid-slots)
   (.kids :initform (c-in nil) ;; most useful
     :owning t
     :accessor kids
     :initarg :kids)
   (registry? :cell nil
     :initform nil
     :initarg :registry?
     :accessor registry?)
   (registry :cell nil
     :initform nil
     :accessor registry)))

(export! registry? )

#+test
(let ((c (find-class 'family)))
  (mop::finalize-inheritance c)
  (class-precedence-list c))

(defmacro the-kids (& kids)
  `(let ((*parent* self))
     (packed-flat! ~@kids)))

(defmacro c?kids (& kids)
  `(c? (the-kids ~@kids)))

(export! c?kids .siblings)

(defmacro s-sib-no () `(position self (kids .parent)))

(defmacro gpar ()
  `(fm-grandparent self))

(defmacro nearest (self-form type)
   (let ((self (gensym)))
   `(bwhen (,self ,self-form)
       (if (typep ,self ',type) ,self (upper ,self ,type)))))

(defn kid1 (self) (car (kids self)))

(defn first-born-p (self)
  (eq self (kid1 .parent)))

(export! first-born-p .kid1p)

(defn kid2 (self) (cadr (kids self)))

(defn last-kid (self) (last1 (kids self)))
(defmacro ^k-last () `(last-kid self))

;; /// redundancy in following

(defmacro psib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (find-prior ,self (kids (fm-parent ,self))))))

(defmacro nsib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (cadr (member ,self (kids (fm-parent ,self)))))))


(export! .psib .nsib)

(defn md-be-adopted (self &aux (fm-parent (fm-parent self)) (selftype (type-of self))) 
  (c-assert self)
  (c-assert fm-parent)
  (c-assert (typep fm-parent 'family) () "fm-parent ~a offered for kid ~a not a family" fm-parent self)
  
  (trc nil "md be adopted >" :kid self (adopt-ct self) :by fm-parent)
  
  (when (plusp (adopt-ct self))
    (c-break "2nd adopt ~a, by ~a" self fm-parent))

  (incf (adopt-ct self))
  (trc nil "getting adopted" self :by fm-parent)
  (bwhen (kid-slots-fn (kid-slots (fm-parent self)))
    (dolist (ks-def (kid-slots-fn self) self)
      (let ((slot-name (ks-name ks-def)))
        (trc nil "got ksdef " slot-name (ks-if-missing ks-def))
        (when (md-slot-cell-type selftype slot-name)
          (trc nil "got cell type " slot-name )
          (when (or (not (ks-if-missing ks-def))
                  (and (null (c-slot-value self slot-name))
                    (null (md-slot-cell self slot-name))))
            (trc nil "ks missing ok " slot-name)
            (multiple-value-bind (c-or-value suppressp)
                ((ks-rule ks-def) self)
              (unless suppressp
                (trc nil "md-install-cell " slot-name c-or-value)
                (md-install-cell self slot-name c-or-value)))))))))

(defobserver .kids :around ((self family) new-kids old-kids)
  
  (c-assert (listp new-kids) () "New kids value for ~a not listp: ~a ~a" self (type-of new-kids) new-kids)
  (c-assert (listp old-kids))
  (c-assert (not (member nil old-kids)))
  (c-assert (not (member nil new-kids)))

  (loop for newk in new-kids
        unless (typep newk 'model)
        do (break "family-kids-obs> non-model ~s typed ~s offered as kid to family ~a"
             newk (type-of newk) self))

  (loop for newk in new-kids
      unless (fm-parent newk)
      do (break "New as of Cells3: parent ~s must be supplied to make-instance of ~s/~s"
           self newk (type-of newk)))

  (call-next-method))

(defmethod kids ((other model-object))  nil)



;------------------  kid slotting ----------------------------
;
(comment ;; defstruct (kid-slotdef
           (:conc-name nil))
  ks-name
  ks-rule
  (ks-if-missing t))

(defmacro mk-kid-slot ((ks-name &key if-missing) ks-rule)
   `(make-kid-slotdef
     :ks-name ',ks-name
     :ks-rule (fn (self)
                 (declare (ignorable self))
                 ,ks-rule)
     :ks-if-missing ,if-missing))

(defmacro def-kid-slots (& slot-defs)
  `(fn (self)
     (declare (ignorable self))
     (list ~@slot-defs)))

; --- registry "namespacing" ---

(defmethod registry? (other) (declare (ignore other)) nil)

(defmethod initialize-instance :after ((self family) &key)
  (when (registry? self)
    (assert (null (registry self)))
    (setf (registry self) (make-hash-table :test 'eq))))

(defmethod fm-register (self &optional (guest self))
  (assert self () "fm-register: nil self registering ~a" guest)
  (if (registry? self)
      (progn
        (trc nil "fm-registering!!!!!!!!!!" (md-name guest) guest :with self)
        (assert (registry self) () "fm-register no reg ~a" self)
        (setf (gethash (md-name guest) (registry self)) guest))
    (fm-register (fm-parent self) guest)))

(defmethod fm-check-out (self &optional (guest self))
  (assert self () "oops ~a ~a ~a" self (fm-parent self) (slot-value self '.fm-parent))
  (if (registry? self)
      (progn
        (assert (registry self) () "fm-check-out no reg ~a" self)
        ;; (trc "removing registered" (md-name guest) :from self)
        (remhash (md-name guest) (registry self)))
    (bif (p (fm-parent self))
      (fm-check-out p guest)
      (brk "oops ~a ~a ~a" self (fm-parent self) (slot-value self '.fm-parent)))))

(defmethod fm-find-registered (id self &optional (must-find? self  must-find?-supplied?))
  (or (if (registry? self)
          (progn
            (assert (registry self) () "fm-find-registered no reg ~a" self)
            (or (gethash id (registry self))
              (prog1 nil
                (when must-find?
                  (loop for k being the hash-keys of (registry self)
                      do (print `(:seeking ,id :see-only ,k :in-registry ,self)))))))
        (bwhen (p (fm-parent self))
          (fm-find-registered id p must-find?)))
    (when (and must-find? (not must-find?-supplied?))
      (loop for k being the hash-keys of (registry (fm-ascendant-if self 'registry?))
            do (print `(registered ,k)))
      (error "fm-find-registered failed seeking ~a starting search at node ~a registry ~a" id self
        (fm-ascendant-if self 'registry?)))))

(export! rg? rg! fm-dump-lineage)

(defmacro rg? (id)
  `(fm-find-registered ,id self nil))

(defmacro rg! (id)
  (if (cl/find id '(:buddy :coach))
      `(fm-other? ,id)
    `(fm-find-registered ,id self)))

(defn fm-dump-lineage (self tag &optional (s t))
  (when self
    (print (list tag self
             (when (typep self 'model)
               (md-state self)) s))
    (fm-dump-lineage .pa tag)) s)
      


               