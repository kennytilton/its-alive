(ns tiltontec.its-alive.cell-types
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]))

(comment
  (defstruct (cell (:conc-name c-))
    model
    slot-name
    value
    input?
    synaptic
    (caller-store (make-fifo-queue) :type cons) ;; (C3) notify callers FIFO
    (state :nascent :type symbol) ;; :nascent, :awake, :optimized-away
    (value-state :unbound :type symbol) ;; :unbound | :unevaluated | :uncurrent | :valid
    ;; uncurrent (aka dirty) new for 06-10-15. we need this so
    ;; c-quiesce can force a caller to update when asked
    ;; in case the owner of the quiesced cell goes out of existence
    ;; in a way the caller will not see via any kids dependency. Saw
    ;; this one coming a long time ago: depending on cell X implies
    ;; a dependency on the existence of instance owning X
    (pulse 0 :type fixnum)
    (pulse-last-changed 0 :type fixnum) ;; lazys can miss changes by missing
    ;; change of X followed by unchange of X in subsequent DP

    (pulse-observed 0 :type fixnum)
    lazy
    (optimize t)
    debug
    md-info))

(defonce ia-types (-> (make-hierarchy)
                      (derive ::model ::object)
                      (derive ::cell ::object)
                      (derive ::c-formula ::cell)))

(defn ia-type? [it typ]
  (println :iaty-chk (type it) it typ)
  (isa? ia-types (type it) typ))

(isa? ia-types ::c-formula ::cell)
(defn c-ref? [x]
  (and (instance? clojure.lang.Ref x)
       (ia-type? @x ::cell)))

;; --- defmodel rizing ---------------------
 
(defn md-ref? [x]
  (and (instance? clojure.lang.Ref x)
       (ia-type? @x ::model)))

;; (isa? ia-types ::c-formula ::cell)

;; (descendants ia-types ::cell)

(def-rmap-slots c-
  slot state input? rule pulse pulse-last-changed pulse-observed
  useds callers optimize value ephemeral? optimized-away?
  lazy synaptic?)

(defn c-model [rc]
  (:me @rc))

(defn c-slot-name [rc]
  (:slot @rc))

(defn c-value-state [rc]
  (when-let [v (c-value rc)]
    (cond
     (= v unbound) :unbound
     (= v unevaluated) :unevaluated
     (= v uncurrent) :uncurrent
     :else :valid)))

(defn c-unbound? [rc]
  (= :unbound (c-value-state rc)))

(defn c-valid? [rc]
  (= :valid (c-value-state rc)))

(defn caller-ensure [used new-caller]
  (alter used assoc :callers (conj (c-callers used) new-caller)))

(defn caller-drop [used caller]
  (alter used assoc :callers (disj (c-callers used) caller)))

; --- model awareness ---------------------------------

(defn mdead?-dispatch [me]
  (assert (md-ref? me))
  [(type me)])

(defmulti mdead? mdead?-dispatch)

(defmethod mdead? :default [me]
  false)

; -----------------------------------------------------
(comment
  (defstruct (c-ruled
              (:include cell)
              (:conc-name cr-))
    (code nil :type list) ;; /// feature this out on production build
    rule))

(comment
  (defmethod c-print-value ((c c-ruled) stream)
    (format stream "%s" (cond ((c-valid? c) (cons (c-value c) "<vld>"))
                              ((c-unbound? c) "<unb>")
                              ((not (c-current? c)) "dirty")
                              (t "<err>"))))

  (defmethod c-print-value (c stream)
    (declare (ignore c stream))))

:cell-types-ok
