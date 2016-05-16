(ns tiltontec.its-alive.model-base
  (:require
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   [tiltontec.its-alive.evaluate :refer :all]
   [tiltontec.its-alive.integrity :refer :all]
   [tiltontec.its-alive.observer :refer :all]
   [tiltontec.its-alive.cells :refer :all]
   ))

(def-rmap-slots md-
  name)

(def-rmap-meta-slots md-
  state cz)

(defn md-cell [me slot]
  (slot (:cz (meta me))))

;;; --- md initialization ---

(declare md-awaken)

(defn md-install-cell [me slot c]
  ;; note that c (a misnomer) might not be a Cell
  (cond
    (c-ref? c) (do
                 (alter c assoc
                        :slot slot
                        :me me)
                 (rmap-setf [slot me]
                            (when (c-input? c)
                              (c-value c)))
                 true)
    :else (do
            (rmap-setf [slot me] c)
            false)))
  
(defn md-awaken
  "(1) do initial evaluation of all ruled slots
   (2) call observers of all slots"
  [me]


  (c-assert (= :nascent (md-state me)))
  (rmap-meta-setf [:state me] :awakening)
  ;;(trx :md-awk @me)
  (doall
   (for [slot (keys @me)]
     (let [c (slot (md-cz me))]
       (cond
         c (do (trx :slot-c slot c)
               (c-awaken c))
         :else (do (trx :noslot slot (slot @me) me)
                   (observe slot me (slot @me) unbound nil))))))

  (rmap-meta-setf [:state me] :awake)
  me)

(defn md-get [me slot]
  ;;(trx :md-get!!!!!!! slot (md-name me))
  (if-let [c  (md-cell me slot)]
    (c-get c)
    (slot @me)))


