(ns tiltontec.its-alive.globals
  (:use [tiltontec.its-alive.utility :refer :all]))

(comment

    Cells -- Automatic Dataflow Managememnt

)
(set! *print-level* 3) ;; cells are recursive data for now

;; --- the Cells beef -----------------------
(def +pulse+ (ref 0))
(defn cells-init []
  (dosync
   (ref-set +pulse+ 0)))
(def ^:dynamic *causation* '())
(def ^:dynamic *call-stack* nil)
(def ^:dynamic *depender* nil) 
;; 2008-03-15: *depender* let's us differentiate between the call stack and
;; and dependency. The problem with overloading *call-stack* with both roles
;; is that we miss cyclic reentrance when we use without-c-dependency in a 
;; rule to get "once" behavior or just when fm-traversing to find someone

(def ^:dynamic *defer-changes* false)
(def +client-q-handler+ (atom nil))
(defonce unbound (gensym "unbound-cell-value"))
(defonce unevaluated (gensym "unevaluated-formulaic-value"))
(defonce uncurrent (gensym "uncurrent-formulaic-value"))


(def ^:dynamic *istack* nil)
(def ^:dynamic *not-to-be* false)

(def ^:dynamic *unfinished-business* nil)
(def ^:dynamic *within-integrity* false)


;; --- debug stuff -----------------------------
(def ^:dynamic *finbiz-id* 0)
(def ^:dynamic *c-prop-depth* 0)

(def +c-debug+ (atom false))
(def ^:dynamic +stop+ (atom false)) ;; emergency brake

;; --- procedure division ----------------------

(defn cells-reset
  ([] (cells-reset {}))
  ([options]
   (reset! +c-debug+ (:debug options false))
   (reset! @+pulse+ 0)
   (reset! +client-q-handler+ (:client-queue-handler options))))

(defmacro without-c-dependency [& body]
  `(binding [*depender* nil]
      ~@body))

(defn .cause []
    (first *causation*))

;; --- 19000 ----------------------------------

(defn c-stopper [why]
  (reset! +stop+ why)) ;; in webserver, make sure each thread binds this freshly

(def +c-stopper+ (atom c-stopper))

(defn c-stop
  ([] (c-stop true))
  ([why]
   (@+c-stopper+ why)))

(defn c-stopped []
  @+stop+)

(defmacro un-stopped [& body]
  `(when-not @+stop+
     ~@body))

(defn ustack$ [tag] ;; debug aid
  (str tag "ustack> "(vec (map (fn [c] (:slot @c)) *call-stack*))))

(defn c-assert
  ([assertion] (when-not assertion
                 (err "c-assert anon failed")))
  ([assertion fmt$ & fmt-args]
   (unless +stop+
           (unless assertion
                   (apply #'err format (str "c-assert> " fmt$)
                          fmt-args)))))

(defn c-break [& args]
  (unless +stop+
          (err (apply 'str args))))

(defn c-warn [& args]
  (unless +stop+
          (format "WARNING!!!!!!!!! %s"
                  (apply 'str args))))

;; (defmacro c-warn? (assertion &optional places fmt$ & fmt-args)
;;   (declare (ignorable assertion places fmt$ fmt-args))
;;   #+(or)`(progn) 
;;   `(unless +stop+
;;      (unless ,assertion
;;        ,(if fmt$
;;             `(c-warn ,fmt$ ~@fmt-args)
;;           `(c-warn "failed assertion: %s" ',assertion)))))

;; (defmacro def-c-trace (model-type &optional slot cell-type)
;;   `(defmethod trcp ((self ,(case cell-type
;;                              (:c? 'c-dependent)
;;                              (otherwise 'cell))))
;;      (and (typep (c-model self) ',model-type)
;;        ,(if slot
;;             `(eq (c-slot-name self) ',slot)
;;           `t))))

; -------- cell conditions (not much used) ---------------------------------------------

(comment ;; (define-condition xcell () ;; new 2k0227
  ((cell :initarg :cell :reader cell :initform nil)
   (app-func :initarg :app-func :reader app-func :initform 'bad-cell)
   (error-text :initarg :error-text :reader error-text :initform "<???>")
   (other-data :initarg :other-data :reader other-data :initform "<nootherdata>"))
  (:report (fn (c s)
             (format s " trouble with cell %s in function %s,%s: %s"
               (cell c) (app-func c) (error-text c) (other-data c)))))

(comment ;; (define-condition c-enabling ()
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (fn (condition stream)
                 (format stream "unhandled <c-enabling>: %s" condition)
                 (brk "i say, unhandled <c-enabling>: %s" condition))))

(comment ;; (define-condition c-fatal (xcell)
   ((name :initform :anon :initarg :name :reader name)
    (model :initform nil :initarg :model :reader model)
    (cell :initform nil :initarg :cell :reader cell))
Kennet   (:report (fn (condition stream)
              (format stream "fatal cell programming error: %s" condition)
              (format stream "  : %s" (name condition))
              (format stream "  : %s" (model condition))
              (format stream "  : %s" (cell condition)))))

(comment ;; (define-condition asker-midst-askers (c-fatal)
  ())
;; "see listener for cell rule cycle diagnotics"

(comment ;; (define-condition c-unadopted (c-fatal) ()
   (:report
    (fn (condition stream)
      (format stream "unadopted cell >: %s" (cell condition))
      (format stream " >: often you mis-edit (c? (c? ...)) nesting is error"))))


:globals-ok
