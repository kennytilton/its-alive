(ns tiltontec.its-alive.cells
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.cell-types :refer :all as cty]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))

(set! *print-level* 3)

(defn make-cell [& kvs]
  (let [options (apply hash-map kvs)]
    (ref
     (with-meta
       (merge {:value unbound
               :state :nascent
               :pulse 0
               :pulse-last-changed 0
               :pulse-observed 0
               :callers #{}
               :lazy false ;; not a predicate (can hold, inter alia, :until-asked)
               :ephemeral? false
               :input? true
              }
              options)
       {:type ::tiltontec.its-alive.cell-types/cell}))))

(defn make-c-formula [& kvs]
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
               :pulse-observed 0
               :callers #{}
               :useds #{}
               :lazy false
               :ephemeral? false
               :optimize true ;; this can also be :when-not-nil
               :input? false ;; not redundant: can start with rule, continue as input
              }
              options)
       {:type ::tiltontec.its-alive.cell-types/c-formula}))))

;;___________________ constructors _______________________________
;; I seem to have created a zillion of these, but I normally
;; use just c-in, c?, and c?n (which starts out as c? and becomes c-in).
;; 

(defmacro c-fn-var [[c] & body]
  `(fn [~c]
     (let [~'me (c-model ~c)
           ~'cache (c-value ~c)]
     ~@body)))

(defmacro c-fn [& body]
  `(c-fn-var (~'slot-c#) ~@body))

(defmacro c? [& body]
  `(make-c-formula
    :code '~body
    :value unevaluated
    :rule (c-fn ~@body)))

(defmacro c?+ [[& options] & body]
  `(make-c-formula
    ~@options
    :code '~body
    :value unevaluated
    :rule (c-fn ~@body)))

(defmacro c?+n [& body]
  `(make-c-formula
    :input? true
    :code '~body
    :value unevaluated
    :rule (c-fn ~@body)))

(defmacro c?n [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? true
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_?n [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? true
    :lazy :until-asked
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?n-dbg [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? true
    :debug true
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?n-until [args & body]
  `(make-c-formula
    :optimize :when-value-t
    :code '~body
    :input? true
    :value unevaluated
    :rule (c-fn ~@body)
    ~@args))

(defmacro c?once [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? nil
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c_1 [& body]
  `(make-c-formula
    :code '(without-c-dependency ~@body)
    :input? nil
    :lazy true
    :value unevaluated
    :rule (c-fn (without-c-dependency ~@body))))

(defmacro c?1 [& body]
  `(c?once ~@body))

(defmacro c?dbg [& body]
  `(make-c-formula
    :code '~body
    :value unevaluated
    :debug true
    :rule (c-fn ~@body)))

(defmacro c?_  [[& options] & body]
  `(make-c-formula
    ~@options
    :code '~body
    :value unevaluated
    :lazy true
    :rule (c-fn ~@body)))

(defmacro c_? [[& options] & body]
  "Lazy until asked, then eagerly propagating"
  `(make-c-formula
    ~@options
    :code '~body
    :value unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)))

(defmacro c_?dbg [& body]
  "Lazy until asked, then eagerly propagating"
  `(make-c-formula
    :code '~body
    :value unevaluated
    :lazy :until-asked
    :rule (c-fn ~@body)
    :debug true))

;; hhhhack add validation somewhere of lazy option

(defmacro c-formula [[& kvs] & body]
  `(make-c-formula
    :code '~body ;; debug aid
    :value unevaluated
    :rule (c-fn ~@body)
    ~@keys))

(defn c-in [value & option-kvs]
  (apply make-cell
         (list* :value value
                :input? true
                option-kvs)))

#_
(let [x (c-in 42)]
  (awaken x))

:cells-ok

