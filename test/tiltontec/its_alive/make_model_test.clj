(ns tiltontec.its-alive.make-model-test
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            ;[tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            [tiltontec.its-alive.make-model :refer :all :as md]))

(deftest mm-install-alive
   (let [res (do ;; sync
              (md/make
               :name "Bob"
               :action (c-in nil
                             :ephemeral? true)
               :bogus (c? (if-let [be (md-get me :bogus-e)]
                            (do
                              (trx :bingo-e!!!!!!!! be)
                              (* 2 be))
                            (trx :bogus-no-e (:bogus-e @me))))
               :bogus-e (c-in 21 :ephemeral? true)
               :loc (c? (case (md-get me :action)
                            :leave :away
                            :return :home
                            :missing))))]
     ;; (println :meta (meta @res))
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
     (md-reset! res :action :return)
     (is (= :home (:loc @res)))
    ))

#_
(deftest mm-alive
  (let [res (md/make
             :name "Bob"
             :action (c-in nil
                           :ephemeral? true)
             :loc (c? (case (:action @me)
                        :leave :away
                        :return :home
                        :missing)))]
    (is (= (:name @res) "Bob"))
    (is (= (name res) "Bob"))
    (is (= (:loc @res) :missing))
    (is (= (loc res) :missing))
    (c-reset! (action res) :return)
    (is (= (:loc @res) :home))
    (is (= (loc res) :home))
    ))

