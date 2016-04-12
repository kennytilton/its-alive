(ns tiltontec.its-alive.integrity
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]))

;; --- the pulse ------------------------------

(def ^:dynamic *one-pulse?* false)

(def ^:dynamic *dp-log* false)


(defn data-pulse-next
  ([] (data-pulse-next []))
  ([pulse-info]
   (unless *one-pulse?*
           #_(when *dp-log*
               (trx "dp-next> " (inc @+pulse+) pulse-info))
           (alter +pulse+ inc)))) ;; hhack try as commute

#_
(dosync (data-pulse-next))

(defn c-current? [c]
  (= (c-pulse c) @+pulse+))

(defn c-pulse-update [c key]
  (assert (>= @+pulse+ (c-pulse c))
          (format "Current DP %s not GE pulse %s of cell %s"
                  @+pulse+ (c-pulse c) @c))
  (alter c assoc :pulse @+pulse+))

;; --- ufb utils ----------------------------

(def +ufb-opcodes+ [:tell-dependents
                        :awaken
                        :client
                        :ephemeral-reset
                        :change])

(def unfin-biz
  ;; no nested finbiz allowed as of now, so just
  ;; build it and in use fill the queues, ufb -do them, and
  ;; ensure they are empty before continuing.
  (into {} (for [i +ufb-opcodes+] [i (ref [])])))

(defn ufb-queue [opcode]
  (or (opcode unfin-biz)
      (err format "ufb-queue> opcode %s unknown" opcode)))

(defn ufb-queue-ensure [opcode]
  "vestigial"
  (or (ufb-queue opcode)
      (err format
           "ufb-queue-ensure now expects all queues to exist. %s does not. we have %s"
           opcode (keys unfin-biz))))

(defn ufb-add [opcode continuation]
  (fifo-add (ufb-queue-ensure opcode) continuation))

(defn ufb-assert-q-empty [opcode]
  (if-let [uqp (fifo-peek (ufb-queue-ensure opcode))]
    (do
      (println :uqp!!!!!!!!!!!!! uqp)
      (err format "ufb queue %s not empty, viz %s"
           opcode uqp))
    true))

;; --- the ufb and integrity beef ----------------------
;;    proper ordering of state propagation


(def ^:dynamic *ufb-do-q* nil) ;; debug aid

(defn ufb-do
  ([opcode]
   (ufb-do (ufb-queue opcode) opcode))

  ([q opcode]
   (println :ufb-do opcode)
   (when-let [[defer-info task] (fifo-pop q)]
     (task opcode defer-info)
     (recur q opcode))))

(defn finish-business []
  (println :fbiz!!!!!)
  (un-stopped
   (loop [tag :tell-dependents]
     (case tag
       :tell-dependents
       (do (ufb-do :tell-dependents)
           (ufb-do :awaken)

           (recur
            (if (fifo-peek (ufb-queue-ensure :tell-dependents))
              :tell-dependents
              :handle-clients)))

        :handle-clients
        (when-let [clientq (ufb-queue :client)]
          (if-let [cqh @+client-q-handler+]
            (cqh clientq)
            (ufb-do clientq :client))
          
          (recur
           (if (fifo-peek (ufb-queue :client))
             :handle-clients
             :ephemeral-reset)))

        :ephemeral-reset
        (do (ufb-do :ephemeral-reset)
            (recur :deferred-state-change))
              
        :deferred-state-change
        (when-let [[defer-info task-fn] (fifo-pop (ufb-queue :change))]
          (data-pulse-next)
          (task-fn :change defer-info)
          (recur :tell-dependents))))))

(defmacro with-integrity [[opcode info] & body]
  `(call-with-integrity
    ~opcode
    ~info
    (fn [~'opcode ~'defer-info]
      ~@body)))

(defmacro with-cc [id &body body]
  `(with-integrity (:change ~id)
     ~@body))

(defmacro without-integrity [& body]
  `(binding
       [*within-integrity* false
        *defer-changes* false
        *call-stack* '()]
     ~@body))

(defn call-with-integrity [opcode defer-info action]
  (when opcode
    (assert (cl-find opcode +ufb-opcodes+)
            (format "Invalid opcode for with-integrity: %s. Allowed values: %s"
                    opcode +ufb-opcodes+)))
  (println :cwi opcode *within-integrity*)
  (un-stopped
   (dosync
    (cond
     (c-stopped) (println :cwi-sees-stop!!!!!!!!!!!)
     
     *within-integrity*
     (if opcode
       (prog1
        :deferred-to-ufb-1
        ;; SETF is supposed to return the value being installed
        ;; in the place, but if the SETF is deferred we return
        ;; something that will help someone who tries to use
        ;; the setf'ed value figure out what is going on:
        (ufb-add opcode [defer-info action]))

       ;; thus by not supplying an opcode one can get something
       ;; executed immediately, potentially breaking data integrity
       ;; but signifying by having coded the with-integrity macro
       ;; that one is aware of this. If you have read this comment.
       (action opcode defer-info))

     :else (binding [*within-integrity* true
                 *defer-changes* false]
             (println :bam-cwi opcode)
             (when (or (zero? @+pulse+)
                       (= opcode :change))
               (data-pulse-next))
             (prog1
              (action opcode defer-info)
              (finish-business)
              (ufb-assert-q-empty :tell-dependents)
              (ufb-assert-q-empty :change)))))))

(defn ephemeral-reset [rc]
  (when (c-ephemeral? rc) ;; air bag
    ;
    ; as of Cells3 we defer resetting ephemerals because everything
    ; else gets deferred and we cannot /really/ reset it until
    ; within finish_business we are sure all callers have been recalculated
    ; and all observers completed (which happens with recalc).
    ;
    (with-integrity (:ephemeral-reset rc)
      (when-let [me (:me @rc)]
        ;; presumption next is that model cells live in
        ;; their own internal slot of model FNYI
        (alter me assoc (:slot @rc) nil))
      (alter rc assoc :value nil))))
:integrity-ok
