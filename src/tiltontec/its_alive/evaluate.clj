(ns tiltontec.its-alive.evaluate
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.propagate :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))

(defn record-dependency [used]
  (when-not (c-optimized-away? used)
    (assert *depender*)
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
       (not (and (ia-type? c :c-formula)
                 (= (c-optimize c) :when-value-t)
                 (nil? (c-value c)))))
  (c-value c)
  ;; --- above we had valid values so did not care. now... -------
  (when-let [md (c-model c)]
    (mdead? (c-model c)))
  (err format "evic> model %s of cell %s is dead" (c-model c) c)

  ;; --- no more early exits  -------------------
  (or (not (c-valid? c))
      (loop [[used & urest] (c-useds c)]
        (when used
          (ensure-value-is-current used :nested c)
          ;; now see if it actually changed
          (or (> (c-pulse-last-changed used)(c-pulse c))
              (recur urest)))))
  (do ;; we seem to need update, but...
    (println :seem-to-need)
    (unless (c-current? c)
            (println :not-current)
            ;; happens if dependent changed and its observer read/updated me
            (calculate-and-set c :evic ensurer))
    (c-value c))

  ;; we were behind the pulse but not affected by the changes that moved the pulse
  ;; record that we are current to avoid future checking:
  :else (do (println :just-pulse)
            (c-pulse-update c :valid-uninfluenced)
            (c-value c))))

(defn cell-read [c]
  (prog1
   (with-integrity ()
     (ensure-value-is-current c :c-read nil))

   (when *depender*
     (record-dependency c))))

(declare calculate-and-link
         c-value-assume)

(defn calculate-and-set [c dbgid dbgdata]
  (un-stopped
   (let [raw-value (calculate-and-link c)]
     ;; Lisp Cells allowed rules to return a second value indicating whether or not to propagate
     ;; Let's see if we need that and then work around missing multiple value return
     (unless (c-optimized-away? c)
             (println :cn-set-assuming)
             ;; this check for optimized-away? arose because a rule using without-c-dependency
             ;; can be re-entered unnoticed since that clears *call-stack*. If re-entered, a subsequent
             ;; re-exit will be of an optimized away cell, which we need not sv-assume on...
             (c-value-assume c raw-value nil)))))

(declare unlink-from-used)

(defn calculate-and-link [c]
  (let [*call-stack* (cons c *call-stack*)
        *depender* c
        *defer-changes* true]
    ;; redecide dependencies each invocation based on actual use
    ;; unsubscribe from dependencies, then clear own record of them
    (unlink-from-used c)
    ((c-rule c) c)))

;;; --- awakening ------------------------------------

(defn awaken-cell-dispatch [c] (type c))

(defmulti awaken-cell (fn [c] (type c)))

(defn awaken-cell-reset []
  (remove-all-methods awaken-cell)
  (defmethod awaken-cell :default [c]
    (println :awaken-cell-fall-thru  (type @c))))

(defmethod awaken-cell :default [slot me new-val old-val]
  (println :obs-fall-thru  slot (type @me) new-val old-val))

(defmethod awaken-cell ::cell [c]
  (assert (c-input? c))
  ;
  ; nothing to calculate, but every cellular slot should be output
  ;
  (when (> @+pulse+ (c-pulse-observed c)) ;; airbag
    (rmap-setf (:pulse-observed c) @+pulse+)
    (binding [*observe-why* :awaken-cell]
      (observe (c-slot-name c) (c-model c) (c-value c) unbound))
    (ephemeral-reset c)))

(defmethod awaken-cell ::c-ruled [c]
  (binding [*depender* nil]
    (calculate-and-set c :fn-awaken-cell nil)))

;;; --- assume new value ----------------------------
;;; this can be at awakening, on setf, or on recalc

(declare c-absorb-value
         optimize-away?!
         c-no-news
         md-slot-value-store)

(defn c-value-assume [c new-value propagation-code]

  (assert (c-ref? c))

  (without-c-dependency
   (let [prior-value (c-value c)]

        (c-pulse-update c :slotv-assume)

        (when-not
            ;; -- bail if unchanged -----------------------
            (and (not (= propagation-code :propagate))
                 (c-no-news c new-value prior-value))
          ;; 
          ;; --- model maintenance ---
        
          (when (and (c-model c)
                     (c-synaptic? c) )
            (md-slot-value-store (c-model c) (c-slot-name c) new-value))
        
          ;; --- cell maintenance ---
          (rmap-setf (:value c) new-value)
          (rmap-setf (:state c) :awake)
        
          (let [callers (c-callers c)] ;; get a copy before we might optimize away
            (when-let [optimize (and (ia-type? c 'c-formula)
                                     (c-optimize c))]
              (case optimize
                :when-value-t (when (c-value c)
                                (unlink-from-used c))
                true (optimize-away?! c))) ;; so coming propagation has it visible
        
            ;; --- data flow propagation -----------
            (unless (= propagation-code :no-propagate)
                    (propagate c prior-value callers))) 

          new-value))))

;; --- unlinking ----------------------------------------------

(defn unlink-from-used [c]
  (doseq [used (c-useds c)]
    (alter used disj c))
  (rmap-setf (:useds c) #{}))


(defn md-cell-flush [c]
  (when (c-model c)
    (rmap-setf (:cells-flushed)
               (conj (:cells-flushed (c-model c))
                     [(c-slot c)(c-pulse-observed c)]))))

;; --- optimize away ------------------------------------------
;; optimizing away cells who turn out not to depend on anyone 
;; saves a lot of work at runtime.

(defn c-optimize-away?! [c]
  (when (and (ia-type? c ::c-formula)
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
      (c-optimize-away?! caller) ;; rare but it happens when rule says (or .cache ...)
      )))

;----------------- change detection ---------------------------------

(defmulti unchanged-test (fn [me slot]
                           [(when me (type @me)) slot]))
     
(defmethod unchanged-test :default [self slotname]
  (fn [new old]
    (= new old)))

;; (defmacro def-c-unchanged-test ((class slotname) & test)
;;   `(defmethod c-unchanged-test ((self ,class) (slotname (eql ',slotname)))
;;      ~@test))


(defn c-no-news [c new-value old-value]
  ((unchanged-test (c-model c) (c-slot c))
   new-value old-value))

;;; ---  vestigial -----------------------------
;; (defn c-absorb-value [c value]
;;   (typecase c
;;     (c-drifter-absolute (c-value-incf c value 0)) ;; strange but true
;;     (c-drifter (c-value-incf c (c-value c) value))
;;     (t value)))

;; (defmethod c-value-incf (c (envaluer c-envaluer) delta)
;;   (c-assert (c-model c))
;;   (c-value-incf c ((envalue-rule envaluer) c)
;;                  delta))

;; (defmethod c-value-incf (c (base number) delta)
;;   (declare (ignore c))
;;   (if delta
;;     (+ base delta)
;;     base))

:evaluate-ok

