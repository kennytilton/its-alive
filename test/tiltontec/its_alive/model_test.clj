(ns tiltontec.its-alive.model-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            [tiltontec.its-alive.model-base :refer :all]
            [tiltontec.its-alive.model :refer :all :as md]
            [tiltontec.its-alive.family :refer :all :as fm]))


(derive cty/ia-types ::typetest ::cty/model)

(deftest mm-typed
  (let [me (md/make
            :type ::typetest
            :x2 (c-in 2)
            :age (c? (* (md-get me :x2)
                        21)))]
    (is (= 42 (md-get me :age)))
    (is (ia-type? me ::typetest))))

(deftest mm-opti-1
   (let [me (md/make
              :x2 2
              :age (c? (* 21 (md-get me :x2)))
              )]
     (println :meta (meta me))
     (is (= 2 (md-get me :x2)))
     (is (= 42 (md-get me :age)))
     (is (nil? (md-cell me :age)))
     ))

(deftest mm-install-alive
   (let [bct (atom 0)
         res (do ;; sync
              (md/make
               :name "Bob"
               :action (c-in nil
                             :ephemeral? true)
               :bogus (c? (if-let [be (md-get me :bogus-e)]
                            (do
                              (trx :bingo-e!!!!!!!! be @bct)
                              (swap! bct inc)
                              (* 2 be))
                            (trx :bogus-no-e (:bogus-e @me))))
               :bogus-e (c-in 21 :ephemeral? true)
               :loc (c? (case (md-get me :action)
                            :leave :away
                            :return :home
                            :missing))))]
     (println :meta (meta res))
     (is (= (:cz (meta res)) (md-cz res)))
     (is (= 4 (count (md-cz res))))
     (is (every? c-ref? (vals (md-cz res))))
     (is (= #{:action :loc :bogus :bogus-e} (set (keys (md-cz res)))))
     (is (every? #(= res (c-me  %))  (vals (md-cz res))))
     (is (= #{:action :loc :bogus :bogus-e}
             (set (map #(c-slot %) 
                       (vals (md-cz res))))))
     (is (= "Bob" (:name @res)))
     (is (= "Bob" (md-name res)))
     (println :res @res)
     (is (= 42 (:bogus @res)))
     (is (= nil (:bogus-e @res))) ;; ephemeral, so reset to nil silently
     (is (= nil (:action @res)))
     (println :loc (:loc @res))
     (is (= :missing (:loc @res)))
     (is (= 1 @bct))
     (reset! bct 0)
     (md-reset! res :action :return)
     (is (= :home (:loc @res)))
     (is (zero? @bct))
    ))




(deftest hello-model
  (let [uni (md/make
             ::fm/family
             :kids (c? (the-kids
                        (md/make
                         :name :visitor
                         :moniker "World"
                         :action (c-in nil 
                                       :ephemeral? true
                                       :obs (fn [slot me new old c]
                                              (when new (trx visitor-did new)))))
                        (md/make
                         :name :resident
                         :action (c-in nil :ephemeral? true)
                         :location (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                                        (case (md-get me :action)
                                          :leave :away
                                          :return :home
                                          :missing))
                         :response (c?+ [:obs (fn-obs (when new
                                                        (trx :r-response new)))
                                         :ephemeral? true]
                                        (when (= :home (md-get me :location))
                                          (when-let [act (mdv! :visitor :action)]
                                            (case act
                                              :knock-knock "hello, world")))))
                        (md/make
                         :name :alarm
                         :on-off (c?+ [:obs (fn-obs
                                             (trx :telling-alarm-api new))]
                                      (if (= :home (mdv! :resident :location)) :off :on))
                         :activity (c?+ [:obs (fn-obs
                                               (case new
                                                 :call-police (trx :auto-dialing-911)
                                                 nil))]
                                        (when (= :on (md-get me :on-off))
                                          (when-let [action (mdv! :visitor :action)]
                                            (case action
                                              :smashing-window :call-police
                                              nil))))))))]
    (let [viz (fm! :visitor uni)
          rez (fm! :resident uni)]
      (is (not (nil? viz)))
      (is (not (nil? rez)))
      (is (not (nil? (md-cell rez :action))))
      (is (= :missing (mdv! :resident :location uni)))
      (md-reset! viz :action :knock-knock)
      (md-reset! viz :action :smashing-window)
      (is (not (nil? (md-cell rez :action))))
      (md-reset! rez :action :return)
      (is (= :home (mdv! :resident :location uni)))
      (md-reset! viz :action :knock-knock))))
