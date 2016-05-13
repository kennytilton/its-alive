(ns tiltontec.its-alive.make-model
  (:require
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   [tiltontec.its-alive.evaluate :refer :all]
   [tiltontec.its-alive.integrity :refer :all]
   [tiltontec.its-alive.observer :refer :all]
   ))

(def-rmap-slots md-
  name)

(def-rmap-meta-slots md-
  state cz)

;;; --- accessors ----
(defn md-cell [me slot]
  (slot (:cz (meta me))))

(defn md-get [me slot]
  ;;(trx :md-get!!!!!!! slot (md-name me))
  (if-let [c  (md-cell me slot)]
    (c-get c)
    (slot @me)))

(defn md-reset! [me slot new-value]
  ;;!(trx :md-reset!!!!!!! slot (md-name me) new-value)
  (if-let [c  (slot (:cz (meta me)))]
    (c-reset! c new-value)
    (do
      (err format "change to slot %s not mediated by cell" slot)
      (rmap-setf [slot me] new-value))))

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

(defn make [& iargs]
  (dosync
   (let [me (ref (->> iargs
                      (partition 2)
                      (filter (fn [[slot v]]
                                (not (= :type slot))))
                      (map (fn [[k v]]
                             (vector k (if (c-ref? v)
                                         unbound
                                         v))))
                      (into {}))
                 :meta (merge {:state :nascent}
                              (->> iargs
                                   (partition 2)
                                   (filter (fn [[slot v]]
                                             (= :type slot)))
                                   (map vec)
                                   (into {}))))]
     (assert (meta me))
     (rmap-meta-setf
      [:cz me]
      (->> iargs
           (partition 2)
           (filter (fn [[slot v]]
                     (md-install-cell me slot v)))
           (map vec)
           (into {})))
     (with-integrity (:awaken me)
       (md-awaken me)))))
  
(defn md-awaken
  "(1) do initial evaluation of all ruled slots
   (2) call observers of all slots"
  [me]

  (c-assert (= :nascent (md-state me)))
  (rmap-meta-setf [:state me] :awakening)
  ;;(trx :md-awk @me)
  (doall
   (for [slot (keys @me)]
     (do ;;(trx :md-awk-slot slot)
         (if-let [c (slot (md-cz me))]
           (c-awaken c)
           (observe slot me (slot @me) unbound nil)))))

  (rmap-meta-setf [:state me] :awake)
  me)

