(ns tiltontec.its-alive.evaluate
  (:require
      [clojure.set :refer [difference]]
      [tiltontec.its-alive.utility :refer :all]
      [tiltontec.its-alive.globals :refer :all]
      [tiltontec.its-alive.cell-types :refer :all]
      [tiltontec.its-alive.observer :refer :all]
      [tiltontec.its-alive.integrity :refer :all]))

(set! *print-level* 3)

(defn record-dependency [used]
  (when-not (c-optimized-away? used)
    (assert *depender*)
    (trx nil :reco-dep!!! :used (c-slot used) :caller (c-slot *depender*))
    (rmap-setf (:useds *depender*)
               (conj (c-useds *depender*) used))
    (caller-ensure used *depender*)))

(declare calculate-and-set )

(defn ensure-value-is-current [c debug-id ensurer]
  ;;
  ;; ensurer can be used cell propagating to callers, 
  ;; or an existing caller who wants to make sure
  ;; dependencies are up-to-date before deciding if it itself is up-to-date.
  ;; just there for debugging.
  ;;
  ;; rest is a hot mess (lots of edge cases) but key to data integrity.
  ;;

  (cond
   ; --------------------------------------------------
   *not-to-be*                          ; we got kicked off during not-to-be processing
                                        ; just return what we have if valid, else nil
   (cond
    (c-unbound? c)
    (err "evic> unbound slot %s of model %s"
         (c-slot c)(c-model c))
    
    (c-valid? c) ;; probably accomplishes nothing
     (c-value c))
  ;; --- easy way out --------------------------
  (c-current? c)
  (c-value c)

  ;; --- also easy with an optimize edge case lost to history -------
  (and (c-input? c)
       (c-valid? c) ;; a c?n (ruled-then-input) cell will not be valid at first
       (not (and (c-formula? c)
                 (= (c-optimize c) :when-value-t)
                 (nil? (c-value c)))))
  (c-value c)
  ;; --- above we had valid values so did not care. now... -------
  (when-let [md (c-model c)]
    (mdead? (c-model c)))
  (err format "evic> model %s of cell %s is dead" (c-model c) c)

  ;; --- no more early exits  -------------------
  (or (not (c-valid? c))
      (loop [[used & urest] (seq (c-useds c))]
        (when used
          (ensure-value-is-current used :nested c)
          ;; now see if it actually changed
          (or (> (c-pulse-last-changed used)(c-pulse c))
              (recur urest)))))
  (do ;; we seem to need update, but...
    ;; (trx nil :seem-to-need)
    (unless (c-current? c)
            (trx nil :not-current-so-calc)
            ;; happens if dependent changed and its observer read/updated me
            (calculate-and-set c :evic ensurer))
    (c-value c))

  ;; we were behind the pulse but not affected by the changes that moved the pulse
  ;; record that we are current to avoid future checking:
  :else (do ;(trx nil :just-pulse)
            (c-pulse-update c :valid-uninfluenced)
            (c-value c))))


(defn c-get [c]
  ;; (trx :c-get :entry (:slot @c))
  (prog1
   (with-integrity ()
     (let [prior-value (c-value c)]
       (prog1
        (ensure-value-is-current c :c-read nil)
        ;; this is new here, intended to awaken standalone cells JIT
        (when (and (= (c-state c) :nascent)
                   (> @+pulse+ (c-pulse-observed c)))
          (rmap-setf (:state c) :awake)
          (c-observe c prior-value :c-get)))))
   (when *depender*
     (record-dependency c))))

(declare calculate-and-link
         c-value-assume)


(defn calculate-and-set [c dbgid dbgdata]
  (trx nil :calc-n-set (c-slot c) dbgid)
  (do ;; wtrx (0 1000 "calc-n-set-entry" (c-slot c) dbgid)
        (un-stopped
         (let [raw-value (calculate-and-link c)]
           ;; Lisp Cells allowed rules to return a second value indicating whether or not to propagate
           ;; Let's see if we need that and then work around missing multiple value return
           (unless (c-optimized-away? c)
                   ;; (trx nil :cn-set-assuming)
                   ;; this check for optimized-away? arose because a rule using without-c-dependency
                   ;; can be re-entered unnoticed since that clears *call-stack*. If re-entered, a subsequent
                   ;; re-exit will be of an optimized away cell, which we need not sv-assume on...
                   (c-value-assume c raw-value nil false))))))

(declare unlink-from-used)

(defn calculate-and-link [c]
  (binding [*call-stack* (cons c *call-stack*)
            *depender* c
            *defer-changes* true]
    ;; redecide dependencies each invocation based on actual use
    ;; unsubscribe from dependencies, then clear own record of them
    (unlink-from-used c :pre-rule-clear)
    (assert (c-rule c) (format "No rule in %s type %s" (:slot c)(type @c)))
    ((c-rule c) c)))

;;; --- awakening ------------------------------------

(defn awaken-cell-dispatch [c] (type c))

(defmulti awaken-cell (fn [c] (type c)))

(defn awaken-cell-reset []
  (remove-all-methods awaken-cell)
  (defmethod awaken-cell :default [c]
    #_ (trx nil :awaken-cell-fall-thru  (type @c))))

(defmethod awaken-cell :default [slot me new-val old-val]
  #_ (trx nil :obs-fall-thru  slot (type @me) new-val old-val))

(defmethod awaken-cell ::cell [c]
  (assert (c-input? c))
  ;
  ; nothing to calculate, but every cellular slot should be output
  ;
  (when (> @+pulse+ (c-pulse-observed c))
    (c-observe c :awaken-cell)
    (ephemeral-reset c)))

(defmethod awaken-cell ::c-ruled [c]
  (binding [*depender* nil]
    (calculate-and-set c :fn-awaken-cell nil)))

;;; --- assume new value ----------------------------
;;; this can be at awakening, on setf, or on recalc

(declare c-absorb-value
         optimize-away?!
         propagate
         c-value-changed?
         md-slot-value-store)

(defn c-reset! [c new-value]
  (if *defer-changes*
    (throw (Exception. "c-reset!> change to %s must be deferred by wrapping it in WITH-INTEGRITY"
                       (c-slot c)))
    (dosync
     ;; (with-integrity (:change (c-slot c))
     (c-value-assume c new-value nil true))))

(defn c-value-assume [c new-value propagation-code initiate-integrity?]
  (assert (c-ref? c))
  (do ;; wtrx (0 1000 :cv-ass (:slot @c) new-value)
        (prog1 new-value ;; sans doubt
               (without-c-dependency
                (let [prior-value (c-value c)
                      prior-state (c-value-state c)]

                  ;; --- cell maintenance ---
                  ;; hhhack: new for 4/19/2016: even if no news at
                  ;; least honor the reset!
                  ;;
                  (rmap-setf (:value c) new-value)
                  (rmap-setf (:state c) :awake)
                  ;; 
                  ;; --- model maintenance ---
                  (when (and (c-model c)
                             (c-synaptic? c) )
                    (md-slot-value-store (c-model c) (c-slot-name c) new-value))
                  
                  (c-pulse-update c :slotv-assume)
                  
                  (when (or (= propagation-code :propagate) ;; forcing
                            (not (some #{prior-state} [:valid :uncurrent]))
                            (c-value-changed? c new-value prior-value))
                    ;; --- something happened ---
                    ;; we may be overridden by a :no-propagate below, but anyway
                    ;; we now can look to see if we can be optimized away
                    (trx nil :sth-happened-to (c-slot c) (c-value c) new-value)
                    (let [callers (c-callers c)] ;; get a copy before we might optimize away
                      (trx nil :sigh (type @c) (c-optimize c))
                      (when-let [optimize (and (c-formula? c)
                                               (c-optimize c))]
                        (trx nil :wtf optimize)
                        (case optimize
                          :when-value-t (when (c-value c)
                                          (trx nil :when-value-t (c-slot c))
                                          (unlink-from-used c :when-value-t))
                          true (optimize-away?! c))) ;; so coming propagation has it visible
                      
                      ;; --- data flow propagation -----------
                      (unless (= propagation-code :no-propagate)
                              (trx nil :prop-by (c-slot c) :to
                                   (when-let [c1 (first callers)]
                                     (c-slot c1)))
                              (let [pfn #(propagate c prior-value callers)]
                                (if initiate-integrity?
                                  (with-integrity (:change :c-reset!) (pfn))
                                  (pfn)))))))))))

;; --- unlinking ----------------------------------------------
(defn unlink-from-used [c why]
  (trx nil :unlinking!!! (c-slot c) why)
  (for [used (c-useds c)]
    (do
        (rmap-setf (:callers used) (disj (c-callers used) c))))

  (rmap-setf (:useds c) #{}))


(defn md-cell-flush [c]
  (when (c-model c)
    (rmap-setf (:cells-flushed)
               (conj (:cells-flushed (c-model c))
                     [(c-slot c)(c-pulse-observed c)]))))

;; --- optimize away ------------------------------------------
;; optimizing away cells who turn out not to depend on anyone 
;; saves a lot of work at runtime.

(defn optimize-away?! [c]
  (when (and (c-formula? c)
             (nil? (c-useds c))
             (c-optimize c)
             (not (c-optimized-away? c)) ;; c-streams (FNYI) may come this way repeatedly even if optimized away
             (c-valid? c) ;; /// when would this not be the case? and who cares?
             (not (c-synaptic? c)) ;; no slot to cache invariant result, so they have to stay around)
             (not (c-input? c)) ;; yes, dependent cells can be inputp
             )
    (rmap-setf (:state c) :optimized-away)
    (when-let [me (c-model c)]
      (rmap-setf (:cells me) (dissoc (:cells @me) (c-slot c)))
      (md-cell-flush c))
    
    ;; let callers know they need not check us for currency again
    (doseq [caller (seq (c-callers c))]
      (alter caller assoc :useds (remove #{c} (c-useds caller)))
      (caller-drop c caller)
      ;;; (trc "nested opti" c caller)
      (optimize-away?! caller) ;; rare but it happens when rule says (or .cache ...)
      )))


;----------------- change detection ---------------------------------

(defmulti unchanged-test (fn [me slot]
                           [(when me (type @me)) slot]))
     
(defmethod unchanged-test :default [self slotname] =)

(defn c-value-changed? [c new-value old-value]
  (trx nil :unchanged? (:slot @c) new-value old-value)
  (not ((or (:unchanged-if @c)
            (unchanged-test (c-model c) (c-slot c)))
        new-value old-value)))

;; (defmethod c-value-incf (c (envaluer c-envaluer) delta)
;;   (c-assert (c-model c))
;;   (c-value-incf c ((envalue-rule envaluer) c)
;;                  delta))

;; (defmethod c-value-incf (c (base number) delta)
;;   (declare (ignore c))
;;   (if delta
;;     (+ base delta)
;;     base))

;--------------- propagate  ----------------------------
; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.

(def ^:dynamic *custom-propagater* nil)

(declare propagate-to-callers
         md-slot-owning?
         md-slot-cell-flushed
         not-to-be)

(defn propagate [c prior-value callers]
  (trx nil :propagate (:slot @c))

  (cond
   *one-pulse?* (when *custom-propagater*
                  (*custom-propagater* c prior-value))
   ;; ----------------------------------
   :else
   (do
     (rmap-setf (:pulse-last-changed c) @+pulse+)
     
     (binding [*depender* nil
               *call-stack* nil
               *c-prop-depth*  (inc *c-prop-depth*)
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
         (when-let [ownees (difference (set-ify prior-value) (set-ify (c-value c)))]
           (doseq [ownee ownees]
             (not-to-be ownee))))

       (propagate-to-callers c callers)
       (trx nil :obs-chkpulse!!!!!!!! @+pulse+ (c-pulse-observed c))
       (when (or (> @+pulse+ (c-pulse-observed c))
                 (some #{(c-lazy c)}
                       '(:once-asked :always true))) ;; messy: these can get setfed/propagated twice in one pulse+
         (c-observe c prior-value :propagate))
       
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
       (ephemeral-reset c)))))

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
  ;; not needed until we are doing dataflow
  (when-not (empty? callers)
    (let [causation (cons c *causation*)] ;; closed over below
      (with-integrity (:tell-dependents c)
        (if (mdead? (c-model c))
          (do (trx "WHOAA!!!! dead by time :tell-deps dispatched; bailing" c))
          (binding [*causation* causation]
            (doseq [caller (seq callers)]
              (trx nil :prop-to-caller (c-slot caller) :by (c-slot c))
              (cond ;; lotsa reasons not to proceed
               (or (= (c-state caller) :quiesced)
                   (some #{(c-lazy caller)} [true :always :once-asked])
                   (and (not (some #{c} (c-useds caller)))
                        (not (c-optimized-away? c)))
                   ;; above: c would have been removed from any c-useds if optimized
                   ;; so caller does not look like a user but it needs one more notification
                   (c-current? caller)) ;; happens if I changed when caller used me in current pulse+
               (trx nil :not-notifying (c-slot caller)
                    (c-current? caller) @+pulse+ (c-pulse caller)
                    )
               :else
               (binding [*trc-ensure* false]
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
;;   `(call-with-one-pulse (fn () ~@body)
;;      ~@(when per-cell? `(:per-cell (fn (c prior-value prior-value-boundp)
;;                                      (declare (ignorable c prior-value prior-value-boundp))
;;                                      ,per-cell)))
;;      ~@(when finally? `(:finally (fn (cs) (declare (ignorable cs)) ,finally)))))

;; (defn call-with-one-datapulse
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
;;   (trx "call-with-one-pulse bumps pulse" @+pulse+)
;;   (finally
;;     (let ((*one-pulse+?* t)
;;           (*custom-propagater* per-cell)
;;           (*the-unpropagated* nil))
;;       (f)
;;       *the-unpropagated*)))
  
:evaluate-ok

