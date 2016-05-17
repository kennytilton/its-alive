(ns tiltontec.its-alive.family
  (:require
      [clojure.set :refer [difference]]
      [tiltontec.its-alive.utility :refer :all]
      [tiltontec.its-alive.cell-types :refer :all :as cty]
      [tiltontec.its-alive.observer :refer :all]
      [tiltontec.its-alive.evaluate :refer :all]
      [tiltontec.its-alive.model-base :refer :all]
      [tiltontec.its-alive.family :refer :all :as fm]
      ))

(derive cty/ia-types ::family ::cty/model)

(def ^:dynamic *par* nil)

(defn fget= [seek poss]
  (assert (any-ref? poss))
  (cond
    (fn? seek) (seek poss)
    (keyword? seek)(do
                     ;; (trx :fget=!!! seek @poss)
                     (= seek (:name @poss)))
    :else (do ;; (trx :fget=-else! seek)
              (= seek poss))))

(defn fget [what where & options]
  ;;(trx :fget-entry what where)
  (when (and where what)
    (let [options (merge {:me? false
                          , :inside? false
                          , :up? true
                          , :wocd? true ;; without-c-dependency
                          } (apply hash-map options))]
      ;;(trx :fget-beef what (md-name where) options)
      (binding [*depender* (if (:wocd? options) nil *depender*)]
        (or (and (:me? options)
                 (fget= what where)
                 where)

            (and (:inside? options)
                 (if-let [kids (md-get where :kids)]
                   (do
                     ;;(trx :fget-inside (:skip options)(doall (map md-name kids)))
                     (if-let [netkids (remove #{(:skip options)} kids)]
                       (do 
                         ;;(trx netkids!!! netkids)
                         (some #(fget what %
                                      :me? true
                                      :inside? true
                                      :up? false) netkids))
                       (trx :no-net-kids)))
                   (trx nil :inside-no-kids @where)))

            (and (:up? options)
                 (when-let [par (:par @where)]
                   ;; (trx :fget-up (:name @par))
                   (fget what par
                         :up? true
                         :me? true
                         :skip where
                         :inside? true)))

            (when (:must? options)
              (err :fget-must-failed what where options)))))))

(defn fm! [what where]
  (fget what where :me? false :inside? true :must? true :up? true))

(defmacro mdv! [what slot & [me]]
  (let [me (or me 'me)]
    `(md-get (fm! ~what ~me) ~slot)))

;; (macroexpand-1 '(mdv! :aa :aa3))

(defmacro the-kids [& tree]
  `(binding [*par* ~'me]
     (remove nil? (flatten (list ~@tree)))))

(defobserver :kids [::family][me newk oldk c]
  (when-not (= oldk unbound)
    (let [lostks (difference (set oldk)(set newk))]
      (trx :lostks (flz lostks))
      (when-not (empty? lostks)
        (trx :bingo-lost! lostks)
        (doseq [k lostks]
          (trx :not-to-eing!!!!! k)
          (not-to-be k))))))

#_
(dosync
 (for [x #{(ref 1)}]
   (not-to-be x)))

:family-ok
