(ns tiltontec.its-alive.observer
  (:use [tiltontec.its-alive.globals :refer :all]))

(ns-unmap *ns* '*observe-why*)
(def ^:dynamic *observe-why* :unspecified)

;; (defparameter *observe-why* nil) ;; debug aid

;; (defgeneric observe (slotname self new old old-boundp cell)
;;   #-(or cormanlisp)
;;   (:method-combination progn))

;; #-cells-testing
;; (defmethod observe #-(or cormanlisp) progn
;;   (slot-name self new old old-boundp cell)
;;   (declare (ignorable slot-name self new old old-boundp cell)))


;;; --- observing --------------------------

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

(defn obs-dispatch [slot-name me new-val old-val]
  (assert (md-ref? me))
  [slot-name (type @me)(type new-val)(type old-val)])

(defmulti observe obs-dispatch)

#_
(obs-reset)

(defn obs-reset []
  (remove-all-methods observe)
  (defmethod observe :default [slot me new-val old-val]
    (println :obs-fall-thru  slot (type @me) new-val old-val)
    ))

(defmethod observe :default [slot me new-val old-val]
  ;; (println :obs-fall-thru  slot (type @me) new-val old-val)
  )

(defn enq-obs
  ([fn]
   ;;(trx enq-obs-entry fn (map #((:slot @%))) *unobserved*)
   (assert fn)
   (assert (fn? fn))
   (swap! *unobserved* conj fn)
   nil)

  ([rjz old-val]
   (assert (c-ref? rjz))
   (let [jz @(ensure rjz)]
     ;; (trx enq-obs2> (:slot jz)(:value jz) old-val)
     (enq-obs (:slot jz) (:me jz) (:value jz) old-val)))

  ([slot me new old]
   (assert (md-ref? me) (str "enq-obs4 me " me " is not an md-ref"))

   ;; (trx enq-obs4> slot new old me)
   (enq-obs (fn []
              ;; (trx act-calling-obs! slot me new old)
              (observe slot me new old)
              ;; (trx obs-fini!!! slot new me)
              nil))))

:observer-ok
