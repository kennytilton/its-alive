(ns com.tiltontec.jellz.integrity-test
  (:require [clojure.test :refer :all]
            [com.tiltontec.jellz.utils :refer :all :as ut]
            [com.tiltontec.jellz.ns :refer :all :as ns]
            [com.tiltontec.jellz.integrity
             :refer :all]))

(deftest ufb-basics
  (let [proof (ref :nope)]
    (dosync
     (ufb-add :client [:hi-mom (fn [code info]
                                 (ref-set proof (list code info)))])
     (is (not (nil? (ufb-queue :client))))
     (ufb-do :client))

    (is (ufb-assert-q-empty :client))
    (is (= @proof [:client :hi-mom]))))

(deftest ufb-order
  (let [done (ref [])
        bang (fn [c]
               (println :bang c)
               (alter done conj c))
        wibang (fn [c]
                 (with-integrity (c)
                   (bang c)))]
    (with-integrity (:awaken)
        (bang :awk)
        (with-integrity (:change)
          (bang :change)
          (wibang :change)
          (wibang :ephemeral-reset)
          (wibang :client)
          (wibang :awaken)
          (wibang :tell-dependents)))
    (println unfin-biz)
    #_(println  @unfin-biz)
    (doseq [k (keys unfin-biz)]
      (println :cking k)
      (is (ufb-assert-q-empty k)))
    (is (= @done [:awk :change :tell-dependents :awaken :client
                  :ephemeral-reset :change]))
    ))


