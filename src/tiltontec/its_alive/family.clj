(ns tiltontec.its-alive.family
  (:require
      ;[clojure.set :refer [difference]]
      [tiltontec.its-alive.utility :refer :all]
      [tiltontec.its-alive.cell-types :refer :all :as cty]
      [tiltontec.its-alive.make-model :refer :all]
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
                          } (apply hash-map options))]
      ;;(trx :fget-beef what (md-name where) options)
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
            (err :fget-must-failed what where options))))))

(defn fm! [what where]
  (fget what where :me? false :inside? true :must? true :up? true))

(defmacro mdv! [what slot & [me]]
  (let [me (or me 'me)]
    `(md-get (fm! ~what ~me) ~slot)))

;; (macroexpand-1 '(mdv! :aa :aa3))
:family-ok

