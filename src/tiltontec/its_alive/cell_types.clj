(ns tiltontec.its-alive.cell-types
  (:require [tiltontec.its-alive.utility :refer :all]))

;; --- the Cells beef -----------------------

(def +pulse+ (ref 0))

(defn cells-init []
  (dosync
   (ref-set +pulse+ 0)))

(def ^:dynamic *causation* '())
(def ^:dynamic *call-stack* nil)
(def ^:dynamic *depender*
  "*depender* let's us differentiate between the call stack and
and dependency. The problem with overloading *call-stack* with both roles
is that we miss cyclic reentrance when we use without-c-dependency in a 
rule to get once behavior or just when fm-traversing to find someone"
  nil)

(def ^:dynamic *defer-changes* false)
(def +client-q-handler+ (atom nil))


(defonce unbound (gensym "unbound-cell-value"))
(defonce unevaluated (gensym "unevaluated-formulaic-value"))
(defonce uncurrent (gensym "uncurrent-formulaic-value"))

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

;; ---------------------------------------------------------

(defonce ia-types (-> (make-hierarchy)
                      (derive ::model ::object)
                      (derive ::cell ::object)
                      (derive ::c-formula ::cell)))

(defn ia-type? [it typ]
  (isa? ia-types (type it) typ))

(defn c-formula? [c]
  (ia-type? c ::c-formula))

(defn c-ref? [x]
  (ia-type? x ::cell))

(def-rmap-slots c-
  me slot state input? rule pulse pulse-last-changed pulse-observed
  useds users callers optimize ephemeral?
  lazy synaptic?)

(defn c-value [c]
  (assert (any-ref? c))
  (cond
    (and (c-ref? c)
         (map? @c)) (:value @c)
    :else @c))

(defn c-optimized-away? [c]
  (cond
    (c-ref? c) (or (not (map? @c))
                   (= :optimized-away (:state @c)))
    :else true))

(defn c-model [rc]
  (:me @rc))

(defn c-slot-name [rc]
  (:slot @rc))

(defn c-value-state [rc]
  (let [v (c-value rc)]
    (cond
      (= v unbound) :unbound
      (= v unevaluated) :unevaluated
      (= v uncurrent) :uncurrent
      :else :valid)))

(defn c-unbound? [rc]
  (= :unbound (c-value-state rc)))

(defn c-valid? [rc]
  (= :valid (c-value-state rc)))

;; --- dependency maintenance --------------------------------

(defn caller-ensure [used new-caller]
  (alter used assoc :callers (conj (c-callers used) new-caller)))

(defn caller-drop [used caller]
  (alter used assoc :callers (disj (c-callers used) caller)))

;; debug aids --------------

(defn c-slots [c k]
  (assert (c-ref? c))
  (set (map c-slot (k @c))))

;; --- defmodel rizing ---------------------

(defn md-ref? [x]
  ;;(trx :md-ref?-sees x)
  (and (instance? clojure.lang.Ref x)
       ;; hhack (ia-type? x ::model)
       ))

(defmulti mdead? (fn [me]
                   (assert (or (nil? me)
                               (md-ref? me)))
                   [(type (when me @me))]))

(defmethod mdead? :default [me]
  false)

(set! *print-level* 3) ;; cells are recursive data for now

(defn md-slot-owning? [class-name slot-name]
  ;; hhack
  false)

:cell-types-ok

