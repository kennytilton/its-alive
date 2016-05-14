(ns tiltontec.its-alive.family-test
  (:require
   [clojure.test :refer :all]
   ;;[clojure.set :refer [difference]]
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   ;;[tiltontec.its-alive.observer :refer :all]
   [tiltontec.its-alive.cells :refer :all :as cz]
   [tiltontec.its-alive.make-model :refer :all :as md]
   [tiltontec.its-alive.family :refer :all :as fm]
   ))


(deftest fm-1
  (is (fget= :bob (ref {:name :bob})))
  (is (not (fget= :bobby (ref {:name :bob}))))
  (is (fget= #(do (println %)
                  (even? @%)) (ref 0)))
  (is (not (fget= #(do (println %)
                       (odd? @%)) (ref 0))))
  )


(deftest fm-2
  (let [u (md/make
           :name :uni
           :kids (c? (vector
                      (md/make
                       :par me
                       :name :aa)
                      (md/make
                       :par me
                       :name :bb
                       :kids (c? (vector
                                  (md/make
                                   :par me
                                   :name :bba)
                                  
                                  (md/make
                                   :par me
                                   :name :bbb)))))))]
    ;; (is (fget :bba u :inside? true :must? true))
    ;; (is (thrown-with-msg?
    ;;      Exception #"fget-must-failed"
    ;;      (fget :bbax u :inside? true :must? true)))
    ;; (is (nil? (fget :bbax u :inside? true :must? false)))
    (let [bba (fget :bba u :inside? true :must? true)]
      (is bba)
      (trx :bba! bba)
      (is (fget :uni bba :inside? true :up? true))
      (is (fget :aa bba :inside? false :up? true))
      (is (fget :bb bba :inside? true :up? true))
      (is (fget :bbb bba :inside? false :up? true))
      )
    ))

(deftest fm-3
  (let [u (md/make
           :u63 (c? (+ (mdv! :aa :aa42)
                       (mdv! :bb :bb21)))
           :kids (c? (vector
                      (md/make
                       :par me
                       :name :aa
                       :aa42 (c? (* 2 (mdv! :bb :bb21)))
                       :aa3 (c-in 3))
                      (md/make
                       :par me
                       :name :bb
                       :bb21 (c? (* 7 (mdv! :aa :aa3)))))))]
    (is (= 63 (md-get u :u63)))
    (is (= 42 (mdv! :aa :aa42 u)))
    (is (= 21 (md-get (fm! :bb u) :bb21)))
    ))
    
                                       
