
(ns tiltontec.its-alive.hello-world-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            [tiltontec.its-alive.model-base :refer :all]
            [tiltontec.its-alive.model :refer :all :as md]
            [tiltontec.its-alive.family :refer :all :as fm]
            ))

(deftest hw-01
  (let [v ;;"visitor"
        {:name "World"
         :action (make-cell :value "knocks"
                            :input? true)}]

    (println (c-get (:name v))
             (c-get (:action v)))

    (is (=  (c-get (:name v)) "World"))
    (is (=  (c-get (:action v)) "knocks"))))

(deftest hw-02
  (let [obs-action (atom nil)
        v ;;"visitor"
        {:name "World"
         :action (c-in "knocks"
                       :slot :v-action
                       :obs ;; short for observer
                       (fn [slot me new old c]
                         (reset! obs-action new)
                         (println :observing slot new old)))}]
    (is (=  (c-get (:name v)) "World"))
    (is (=  (c-get (:action v)) "knocks"))
    (is (= "knocks" @obs-action))))

(deftest hw-03
  (let [action (atom nil)
        obs-action (fn [slot me new old c]
                     (reset! action new)
                     (println :observing slot new old))
        v {:name "World"
           :action (c-in nil :slot :v-action
                         :obs obs-action)}]

    (is (nil? (c-get (:action v))))
    (is (nil? @action))

    (c-reset! (:action v) "knock-knock")
    (is (= "knock-knock" @action))
    (is (= (c-get (:action v)) "knock-knock"))))

(defn gobs
  [slot me new old c]
  (println :gobs> slot new old))

(deftest hw-04
  (let [r-action (c-in nil
                       :slot :r-action
                       :obs gobs)
        r-loc (make-c-formula
               :slot :r-loc
               :obs gobs
               :rule (fn [c]
                       (case (c-get r-action)
                         :leave :away
                         :return :at-home
                         :missing)))]
    (c-awaken r-loc)
    (is (= :missing (:value @r-loc)))
    (println :---about-to-leave------------------)
    (c-reset! r-action :leave)
    (println :---left------------------)
    (is (= :away (c-get r-loc)))))

(deftest hw-5
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (println slot new old))
        v {:name "World"
           :action (c-in nil :slot :v-action
                         :obs obs-action)}
        r-action (c-in nil)
        r-loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                   (case (c-get r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (c?+ [:obs (fn-obs (trx :r-resp new))]
                        (when (= :home (c-get r-loc))
                          (when-let [act (c-get (:action v))]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (c-get r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc)))))

(deftest hello-world
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (println slot new old))
        v {:name "World"
           :action (c-in nil
                         :slot :v-action
                         :ephemeral? true
                         :obs obs-action)}
        r-action (c-in nil)
        r-loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                   (case (c-get r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (c?+ [:obs (fn-obs (trx :r-response new))
                         :ephemeral? true]
                        (when (= :home (c-get r-loc))
                          (when-let [act (c-get (:action v))]
                            (case act
                              :knock-knock "hello, world"))))]
    (is (nil? (c-get r-response)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc)))
    (c-reset! (:action v) :knock-knock)))

(deftest hello-world-2
  (println :--go------------------)
  (let [obs-action (fn [slot me new old c]
                     (when new (trx visitor-did new)))
        v {:name "World"
           :action (c-in nil
                         :slot :v-action
                         :ephemeral? true
                         :obs obs-action)}
        r-action (c-in nil)
        r-loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
                   (case (c-get r-action)
                     :leave :away
                     :return :home
                     :missing))
        r-response (c?+ [:obs (fn-obs (when new
                                        (trx :r-response new)))
                         :ephemeral? true
                         ]
                        (when (= :home (c-get r-loc))
                              (when-let [act (c-get (:action v))]
                                (case act
                                  :knock-knock "hello, world"))))
        alarm (c?+ [:obs (fn-obs
                          (trx :telling-alarm-api new))]
                   (if (= :home (c-get r-loc)) :off :on))
        alarm-do (c?+ [:obs (fn-obs
                            (case new
                              :call-police (trx :auto-dialing-911)
                              nil))]
                     (when (= :on (c-get alarm))
                       (when-let [action (c-get (:action v))]
                         (case action
                           :smashing-window :call-police
                           nil))))]
    (c-awaken [alarm-do r-response r-loc (:action v)])
    (is (= :missing (:value @r-loc)))
    (c-reset! (:action v) :knock-knock)
    (c-reset! (:action v) :smashing-window)
    (c-reset! r-action :return)
    (is (= :home (c-get r-loc))) 
    (c-reset! (:action v) :knock-knock)
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
