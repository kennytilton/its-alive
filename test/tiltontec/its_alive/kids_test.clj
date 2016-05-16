(ns tiltontec.its-alive.kids-test
  (:require
   [clojure.test :refer :all]
   [clojure.set :refer [difference]]
   [tiltontec.its-alive.utility :refer :all]
   [tiltontec.its-alive.cell-types :refer :all :as cty]
   [tiltontec.its-alive.evaluate :refer :all]
   [tiltontec.its-alive.cells :refer :all :as cz]
   [tiltontec.its-alive.model-base :refer :all]
   [tiltontec.its-alive.model :refer :all :as md]
   [tiltontec.its-alive.family :refer :all :as fm]
   ))

(deftest k-notq2be
  (let [f (md/make :type ::fm/family
                   :ee (c-in 2)
                   :kids (c? (the-kids
                              (when (odd? (md-get me :ee))
                                (md/make
                                 :name :yep
                                 :value (c? (* 14 (md-get (:par @me) :ee))))))))]
    (is (ia-type? f ::fm/family))
    (is (empty? (md-get f :kids)))
    
    (do
      (md-reset! f :ee 3)
      (is (not (empty? (md-get f :kids))))
      (is (= 42 (mdv! :yep :value f)))
    
      (let [dmw (first  (md-get f :kids))]
        (assert (md-ref? dmw))
        (md-reset! f :ee 0)
        (is (empty? (md-get f :kids)))
        (trx :dmw dmw @dmw)
        (is (nil? @dmw))
        (is (= :dead (:state (meta dmw))))))))
