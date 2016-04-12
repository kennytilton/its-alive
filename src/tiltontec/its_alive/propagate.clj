(ns tiltontec.its-alive.propagate
  (:require
   [clojure.set :refer [difference]]
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.globals :refer :all]
   [tiltontec.its-alive.cell-types :refer :all]
   [tiltontec.its-alive.observer :refer [observe]]
   [tiltontec.its-alive.integrity :refer [*one-pulse?*
                                          ephemeral-reset]]
   ))


;--------------- propagate  ----------------------------
; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.

(def ^:dynamic *per-cell-handler* nil)

(declare propagate-to-callers
         md-slot-owning?
         md-slot-cell-flushed
         not-to-be)

(defn propagate [c prior-value callers]
  (cond
   *one-pulse?* (when *per-cell-handler*
                  (*per-cell-handler* c prior-value))
   ;; ----------------------------------
   :else
   (do
     (rmap-setf (:pulse-last-changed c) @+pulse+)
     
     (binding [*depender* nil
               *call-stack* nil]
       (let [*c-prop-depth*  (inc *c-prop-depth*)
             *defer-changes* true]
         ;; --- manifest new value as needed ---
         ;;
         ;; 20061030 Trying not.to.be first because doomed instances may be interested in callers
         ;; who will decide to propagate. If a family instance kids slot is changing, a doomed kid
         ;; will be out of the kids but not yet quiesced. If the propagation to this rule asks the kid
         ;; to look at its siblings (say a view instance being deleted from a stack who looks to the psib
         ;; pb to decide its own pt), the doomed kid will still have a parent but not be in its kids slot
         ;; when it goes looking for a sibling relative to its position.
         (when (and prior-value
                    (c-model c)
                    (md-slot-owning? (type (c-model c)) (c-slot-name c)))
           (when-let [lost (difference (set-ify prior-value) (set-ify (c-value c)))]
             (doseq [l1 lost]
               (not-to-be l1))))

         (propagate-to-callers c callers)
         (println :chkpulse!!!!!!!! @+pulse+ (c-pulse-observed c))
         (when (or (> @+pulse+ (c-pulse-observed c))
                   (some #{(c-lazy c)}
                         '(:once-asked :always true))) ;; messy: these can get setfed/propagated twice in one pulse+
           (rmap-setf (:pulse-observed c) @+pulse+)
           (let [*observe-why* :propagate]
             
             (println :observe!!!!!! (c-slot-name c) (c-model c)
                      (c-value c) prior-value c)
             (observe (c-slot-name c) (c-model c)
                      (c-value c) prior-value c)))
         
         ;;
         ;; with propagation done, ephemerals can be reset. we also do this in c-awaken, so
         ;; let the fn decide if C really is ephemeral. Note that it might be possible to leave
         ;; this out and use the pulse to identify obsolete ephemerals and clear them
         ;; when read. That would avoid ever making again bug I had in which I had the reset 
         ;; inside slot-value-observe,
         ;; thinking that that always followed propagation to callers. It would also make
         ;; debugging easier in that I could find the last ephemeral value in the inspector.
         ;; would this be bad for persistent CLOS, in which a DB would think there was still a link
         ;; between two records until the value actually got cleared?
         ;;
         (ephemeral-reset c))))))

(defn propagate-to-callers [c callers]
  ;;
  ;;  We must defer propagation to callers because of an edge case in which:
  ;;    - X tells A to recalculate
  ;;    - A asks B for its current value
  ;;    - B must recalculate because it too uses X
  ;;    - if B propagates to its callers after recalculating instead of deferring it
  ;;       - B might tell H to reclaculate, where H decides this time to use A
  ;;       - but A is in the midst of recalculating, and cannot complete until B returns.
  ;;         but B is busy eagerly propagating. "This time" is important because it means
  ;;         there is no way one can reliably be sure H will not ask for A
  ;;
  #_ ;; not needed until we are doing dataflow
  (when-not (empty? callers)
    (let [causation (cons c *causation*)] ;; closed over below
      (with-integrity (:tell-dependents c)
        (if (mdead? (c-model c))
          (do (trx nil "WHOAA!!!! dead by time :tell-deps dispatched; bailing" c))
          (binding [*causation* causation]
            (doseq [caller (seq callers)]
              (when-not ;; lotsa reasons not to proceed
                  (or (eq (c-state caller) :quiesced)
                      (some #{(c-lazy caller)} [true :always :once-asked])
                      (and (not (some #{c} (c-useds caller)))
                           (not (c-optimized-away? c)))
                      ;; above: c would have been removed from any c-useds if optimized
                      ;; so caller does not look like a user but it needs one more notification
                      (c-current? caller)) ;; happens if I changed when caller used me in current pulse+
                (binding [*trc-ensure* (trcp c)]
                    ;;
                    ;; we just calculate-and-set at the first level of dependency because
                    ;; we do not need to check the next level (as ensure-value-is-current does)
                    ;; because we already know /this/ notifying dependency has changed, so yeah,
                    ;; any first-level cell /has to/ recalculate. (As for ensuring other dependents
                    ;; of the first level guy are current, that happens automatically anyway JIT on
                    ;; any read.) This is a minor efficiency enhancement since ensure-value-is-current would
                    ;; very quickly decide it has to re-run, but maybe it makes the logic clearer.
                    ;;
                    ;;(ensure-value-is-current caller :prop-from c) <- next was this, but see above change reason
                    ;;
                  (calculate-and-set caller :propagate c))))))))))

(def ^:dynamic *the-unpropagated* nil)

;; (defmacro with-one-datapulse+ ((&key (per-cell nil per-cell?) (finally nil finally?)) & body)
;;   `(call-with-one-datapulse+ (fn () ~@body)
;;      ~@(when per-cell? `(:per-cell (fn (c prior-value prior-value-boundp)
;;                                      (declare (ignorable c prior-value prior-value-boundp))
;;                                      ,per-cell)))
;;      ~@(when finally? `(:finally (fn (cs) (declare (ignorable cs)) ,finally)))))

;; (defn call-with-one-datapulse+
;;     (f &key
;;       (per-cell (fn (c prior-value prior-value?)
;;                   (unless (cl-find c *the-unpropagated* :key 'car)
;;                     (pushnew (list c prior-value prior-value?) *the-unpropagated*))))
;;       (finally (fn (cs)
;;                  (print `(finally sees ~@+pulse+ ,cs))
;;                  (loop for (c prior-value prior-value?) in (nreverse cs) do
;;                        (propagate c prior-value)))))
;;   (assert (not *one-pulse+?*))
;;   (data-pulse+-next :client-prop)
;;   (trx "call-with-one-datapulse+ bumps pulse" @+pulse+)
;;   (finally
;;     (let ((*one-pulse+?* t)
;;           (*per-cell-handler* per-cell)
;;           (*the-unpropagated* nil))
;;       (f)
;;       *the-unpropagated*)))
  
:propagate-ok
