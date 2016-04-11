(ns tiltontec.its-alive.constructor
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all as cty]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))
;; (ns-unmap *ns* 'c-model)
(set! *print-level* 3)

;; (defn make-c-input  
;;   ([]
;;    (make-c-input nil))

;;   ([val]
;;    (make-c-input {} val))

;;   ([options val]
;;    (ref (with-meta
;;           (assoc options :value val
;;                  :input? true
;;                  :pulse 0 ;; hhack try init this and last-changed to current pulse
;;                  :pulse-last-changed 0
;;                  :pulse-observed 0
;;                  :state :valid
;;                  :callers {})
;;           {:type ::cty/cell}))))

(defn make-cell [& kvs]
  (let [options (apply hash-map kvs)]
    (ref
     (with-meta
       (merge {:value unbound
               :state :nascent
               :pulse 0
               :pulse-last-changed 0
               :pulse-last-observed 0
               :callers #{}
               :lazy false ;; not a predicate (can hold, inter alia, :until-asked)
               :ephemeral? false
               :input? true
              }
              options)
       {:type :cell}))))

(defn make-c-dependent [& kvs]
  (let [options (apply hash-map kvs)
        rule (:rule options)]
    (assert rule)
    (assert (fn? rule))
    (ref
     (with-meta
       (merge {:value unevaluated
               :state :nascent
               :pulse 0
               :pulse-last-changed 0
               :pulse-last-observed 0
               :callers #{}
               :useds #{}
               :lazy false
               :ephemeral? false
               :optimize true ;; this can also be :when-not-nil
               :input? false ;; not redundant: can start with rule, continue as input
              }
              options)
       {:type :c-formula}))))

(set! *print-level* 2)

;___________________ constructors _______________________________

(defmacro c-fn-var [[c] & body]
  `(fn [~c]
     (let [~'self (c-model ~c)
           .cache (c-value ~c)
           .cache-bound-p (cache-bound-p ~c)]
     ~@body)))

(defmacro c-fn [& body]
  `(c-fn-var (slot-c) ~@body))

(defmacro c? [& body]
  `(make-c-dependent
    :code '~body
    :value unevaluated
    :rule (c-fn ~@body)))

(defmacro c?+n [& body]
  `(make-c-dependent
    :input? t
    :code '~body
    :value unevaluated
    :rule (c-fn ~@body)))

(defmacro c?n [& body]
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :input? t
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_?n [& body]
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :input? t
    :lazy :until-asked
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?n-dbg [& body]
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :input? t
    :debug t
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?n-until [args & body]
  `(make-c-dependent
    :optimize :when-value-t
    :code '~body
    :input? t
    :value unevaluated
    :rule (c-fn ~@body)
    ~@args))

(defmacro c?once [& body]
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :input? nil
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_1 [& body]
  `(make-c-dependent
    :code '(without-c-dependency ~@body)
    :input? nil
    :lazy t
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?1 [& body]
  `(c?once ~@body))

(defmacro c?dbg [& body]
  `(make-c-dependent
    :code '~body
    :value unevaluated
    :debug t
    :rule (c-fn ~@body)))

(defmacro c?_ [& body]
  `(make-c-dependent
    :code '~body
    :value unevaluated
    :lazy t
    :rule (c-fn ~@body)))

(defmacro c_? [& body]
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code '~body
    :value unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)))

(defmacro c_?dbg [& body]
  "Lazy until asked, then eagerly propagating"
  `(make-c-dependent
    :code '~body
    :value unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)
    :debug t))

;; hhhhack add validation somewhere of lazy option

(defmacro c-formula [[& kvs] & body]
  `(make-c-dependent
    :code '~body ;; debug aid
    :value unevaluated
    :rule (c-fn ~@body)
    ~@keys))

(defn c-in [value]
  (make-cell
    :input? true
    :value value))

(comment ;; defstruct (c-envaluer (:conc-name nil))
  envalue-rule)

(defn awaken-cell-dispatch [c]
  [(type @c)])


:constructors-ok
