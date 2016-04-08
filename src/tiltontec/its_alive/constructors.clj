(comment

    Cells -- Automatic Dataflow Managememnt



)


(eval-now!
  (export '(.cache-bound-p

            ;; Cells Constructors
            c?n
            c?once
            c?n-until
            c?1
            c_1
            c?+n

            ;; Debug Macros and Functions
            c?dbg
            c_?dbg
            c-input-dbg

            )))

;___________________ constructors _______________________________

(defmacro c-fn (& body)
  `(c-fn-var (slot-c) ~@body))

(defmacro c-fn-var ([c] & body)
  `(fn (,c &aux (self (c-model ,c))
             (.cache (c-value ,c))
             (.cache-bound-p (cache-bound-p ,c)))
     (declare (ignorable .cache .cache-bound-p self))
     ~@body))

(defmacro with-c-cache ((fn) & body)
  (let ((new (gensym)))
    `(or (bwhen (,new (progn ~@body))
           (,fn ,new .cache))
       .cache)))

;-----------------------------------------

(defmacro c? (& body)
  `(make-c-dependent
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :rule (c-fn ~@body)))

(defmacro c?+n (& body)
  `(make-c-dependent
    :inputp t
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :rule (c-fn ~@body)))

(defmacro c?n (& body)
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :inputp t
    :value-state :unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_?n (& body)
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :inputp t
    :lazy :until-asked
    :value-state :unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(export! c?n-dbg c_?n)

(defmacro c?n-dbg (& body)
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :inputp t
    :debug t
    :value-state :unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?n-until (args & body)
  `(make-c-dependent
    :optimize :when-value-t
    :code #+live nil #-live ',body
    :inputp t
    :value-state :unevaluated
    :rule (c-fn ~@body)
    ~@args))

(defmacro c?once (& body)
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :inputp nil
    :value-state :unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_1 (& body)
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :inputp nil
    :lazy t
    :value-state :unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?1 (& body)
  `(c?once ~@body))

(defmacro c?dbg (& body)
  `(make-c-dependent
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :debug t
    :rule (c-fn ~@body)))

(defmacro c?_ (& body)
  `(make-c-dependent
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :lazy t
    :rule (c-fn ~@body)))

(defmacro c_? (& body)
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)))

(defmacro c_?dbg (& body)
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code #+live nil #-live ',body
    :value-state :unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)
    :debug t))

(defmacro c?? ((&key (tagp nil) (in nil) (out t))& body)
  (let ((result (copy-symbol 'result))
        (thetag (gensym)))
     `(make-c-dependent
       :code #+live nil #-live ',body
       :value-state :unevaluated
       :rule (c-fn
              (let ((,thetag (gensym "tag"))
                    (*trcdepth* (1+ *trcdepth*))
                    )
                (declare (ignorable self ,thetag))
                ,(when in
                   `(trc "c??> entry" (c-slot-name c) (c-model c) (when ,tagp ,thetag)))
                (count-it :c?? (c-slot-name c) (md-name (c-model c)))
                (let ((,result (progn ~@body)))
                  ,(when out `(trc "c?? result:" ,result (c-slot-name c) (when ,tagp ,thetag)))
                  ,result))))))

(defmacro c-formula ((& keys &key lazy &allow-other-keys) & forms)
  (assert (member lazy '(nil t :once-asked :until-asked :always)))
  `(make-c-dependent
    :code ',forms
    :value-state :unevaluated
    :rule (c-fn ~@forms)
    ~@keys))

(defmacro c-input ((& keys) &optional (value nil valued-p))
  `(make-cell
    :inputp t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value
    ~@keys))

(defmacro c-in (value)
  `(make-cell
    :inputp t
    :value-state :valid
    :value ,value))

(export! c-in-lazy c_in)

(defmacro c-in-lazy (& body)
  `(c-input (:lazy :once-asked) (progn ~@body)))

(defmacro c_in (& body)
  `(c-input (:lazy :once-asked) (progn ~@body)))

(defmacro c-input-dbg (&optional (value nil valued-p))
  `(make-cell
    :inputp t
    :debug t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value))

(defmacro c... ((value) & body)
  `(make-c-drifter
    :code #+live nil #-live ',body
    :value-state :valid
    :value ,value
    :rule (c-fn ~@body)))

(defmacro c-abs (value & body)
  `(make-c-drifter-absolute
    :code #+live nil #-live ',body
    :value-state :valid
    :value ,value
    :rule (c-fn ~@body)))


(defmacro c-envalue (& body)
  `(make-c-envaluer
    :envalue-rule (c-fn ~@body)))

