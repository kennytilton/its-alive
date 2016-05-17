(ns tiltontec.its-alive.model
  (:require
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   [tiltontec.its-alive.evaluate :refer :all]
   [tiltontec.its-alive.integrity :refer :all]
   [tiltontec.its-alive.observer :refer :all]
   [tiltontec.its-alive.cells :refer :all]
   [tiltontec.its-alive.model-base :refer :all]
   ;;[tiltontec.its-alive.family :refer :all]
   ))

;;; --- accessors ----

(defn md-reset! [me slot new-value]
  (trx :md-reset!!!!!!! slot (md-name me) new-value)
  (if-let [c  (md-cell me slot)]
    (c-reset! c new-value)
    (do
      (err format "change to slot %s not mediated by cell" slot)
      (rmap-setf [slot me] new-value))))

(defn make [& iargs]
  (cond
    (odd? (count iargs)) (apply make :type iargs)
    :else
    (dosync
     (let [me (ref (merge {:par *par*}
                          (->> iargs
                               (partition 2)
                               (filter (fn [[slot v]]
                                         (not (= :type slot))))
                               (map (fn [[k v]]
                                      (vector k (if (c-ref? v)
                                                  unbound
                                                  v))))
                               (into {})))
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
         (md-awaken me))
       me))))

(def kwt "kenneth.tilton@ktilt.com")
(apply str (reduce (fn [[& as][& xs]]
                     (vec (map str as xs)))
                   (partition 3 kwt)))



