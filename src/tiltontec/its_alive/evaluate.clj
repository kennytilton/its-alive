(ns tiltontec.its-alive.evaluate
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.integrity :refer :all]))

(declare ensure-value-is-current)

    
(defn cell-read [c]
  (prog1
   (with-integrity ()
     (ensure-value-is-current c :c-read nil))
   (when *depender*
     (record-dependency c))))

(defn record-dependency [used]
  (when-not (c-optimized-away? used)
    (assert *depender*)
    (alter *depender* assoc
           :useds (conj (:useds @*depender* #{}) used))
    (caller-ensure used *depender*)))

(declare calculate-and-set)
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
       (not (and (type? c ::c-dependent)
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
    (unless (c-current? c)
            ;; happens if dependent changed and its observer read/updated me
            (calculate-and-set c :evic ensurer))
    (c-value c))

  ;; we were behind the pulse but not affected by the changes that moved the pulse
  ;; record that we are current to avoid future checking:
  :else (do (c-pulse-update c :valid-uninfluenced)
            (c-value c))))

(declare calculate-and-link)

(defn calculate-and-set [c dbgid dbgdata]
  (un-stopped
   (let [raw-value (calculate-and-link c)]
     ;; Lisp Cells allowed rules to return a second value indicating whether or not to propagate
     ;; Let's see if we need that and then work around missing multiple value return
     (unless (c-optimized-away? c)
             ;; this check for optimized-away? arose because a rule using without-c-dependency
             ;; can be re-entered unnoticed since that clears *call-stack*. If re-entered, a subsequent
             ;; re-exit will be of an optimized away cell, which we need not sv-assume on...
             (md-slot-value-assume c raw-value nil)))))

(defn calculate-and-link [c]
  (let [*call-stack* (cons c *call-stack*)
        *depender* c
        *defer-changes* true]
    ;; redecide dependencies each invocation based on actual use
    ;; unsubscribe from dependencies, then clear own record of them
    (doseq [used (c-useds c)]
      (alter used disj c))
    (ref-set c assoc :useds #{})
    ((c-rule c) c)))

;;; --- awakening ------------------------------------

(defn awaken-cell-dispatch [c] (type c))
(defmulti awaken-cell awaken-cell-dispatch)

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
    (ref-setf (:pulse-observed c) @+pulse+)
    (binding [*observe-why* :awaken-cell]
      (observe (c-slot-name c) (c-model c) (c-value c) unbound))
    (ephemeral-reset c)))

(defmethod awaken-cell ::c-ruled [c]
  (binding [*depender* nil]
    (calculate-and-set c :fn-awaken-cell nil)))

:evaluate-ok

