(ns tiltontec.its-alive.observer
  (:use [tiltontec.its-alive.utility :refer :all]
        [tiltontec.its-alive.globals :refer :all]
        [tiltontec.its-alive.cell-types :refer :all]))


(def ^:dynamic *unobserved* [])

(defmacro with-obs [& body]
  `(call-with-obs (fn [] ~@body)))

(defn call-with-obs [fn]
  (binding [*unobserved* (atom [])] ;;(transient [])]
    (let [r (fn)]
     ;; (trx with-obs-body-returned r)
     (let [fns @*unobserved*] ;;(persistent! *unobserved*)]
       ;; (trx with-obs-mopping (count fns))
       (doseq [fn fns]
         (assert fn)
         (assert (fn? fn) (str "with-obs> observer not fn?: " fn))
         (fn)
         ;; (trx obs-returned-ok 42)
         )
       ;; (trx :with-obs-done-mopping 42)
       )
     r)))

(defn obs-dispatch [slot-name me new-val old-val c]
  [slot-name
   (type (when (md-ref? me) @me))
   (type new-val)
   (type old-val)])

(defmulti observe obs-dispatch #_(fn [slot-name me new-val old-val c]
                    [slot-name
                     (type (when (md-ref? me) @me))
                     (type new-val)
                     (type old-val)]))

#_
(obs-reset)

(defn obs-reset []
  (remove-all-methods observe)
  (defmethod observe :default [slot me new-val old-val c]
    ;; (println :obs-fall-thru  slot (type @me) new-val old-val c)
    ))

(defmethod observe :default [slot me new-val old-val c]
  #_(println :obs-fall-thru  slot
           (cond
            (md-ref? me)(type @me)
            :else me)
           new-val old-val c))

(defmacro defobserver [slot types params & body]
  (assert (keyword? slot) "defobserver> slot should be a keyword.")
  (let [ftypes (concat types (take-last (- 3 (count types))
                                        '(::tiltontec.its-alive.cell-types/model Object Object)))
        fparams (concat params
                        (take-last (- 4 (count params))
                                   '(me new-value old-value c)))]
    `(defmethod tiltontec.its-alive.observer/observe [~slot ~@ftypes][~'slot ~@fparams]
       ~@body)))

(defn c-observe
  ([c why]
   (c-observe c unbound why))
  ([c prior-value why]
   (assert (c-ref? c))
   (rmap-setf (:pulse-observed c) @+pulse+)
   ((or (:obs @c) observe)
    (c-slot c)(c-model c)(c-value c) prior-value c)))

:observer-ok
