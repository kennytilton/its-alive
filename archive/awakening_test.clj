(ns com.tiltontec.jellz.awakening-test
  (:require [clojure.test :refer :all]
            [com.tiltontec.jellz.utils :refer :all :as ut]
            [com.tiltontec.jellz.ns :refer :all :as ns]
            [com.tiltontec.jellz.api :refer :all :as api]
            [com.tiltontec.jellz.awakening :refer :all :as awk]))


;;; ------------------------------------------------
;;; some fundamental checks of mechanics
;;

(defmd ::abbey [])

(deftest defmd-basics
  (is (isa? ::abbey ::ns/model))
  (let [it (mkjzo ::abbey :aa 42)]
    (is (= ::abbey (type @it)))
    (is (isa? (type @it) ::ns/model))
    (is (md-ref? it))))

(def obs-proof (atom nil))

(defobserver :monk [::abbey String][]
  (reset! obs-proof new-val))

(deftest obs-called-multi-ok
    (reset! obs-proof false)

    (let [me (mkjzo ::abbey :monk 42)] ;; 42 isnota String
      (is (= @obs-proof false)))

    (let [me (mkjzo ::abbey :monk "Cool")]
      (is (= @obs-proof "Cool"))))

;; ------- baby steps ---------------------------
;; First up: just a simple test that the engine can calculate
;; and observe at all. Baby step one, but it was how Cells
;; got invented: we were working on ordering the calculation
;; of the interdependent positions and sizes of view elements.
;;
(defmd ::thing-1 [])

(def-jzo-slots aa bb cc)

(deftest simple-but-nested-calculation
  (let [cco (atom false)
        set-cco (fn [slot me new old]
                  (reset! cco new))

        t-1  (mkjzo ::thing-1
                 :aa 1
                 :bb (jzi 3)
                 :cc (jzf+ {:obs set-cco} (+ (aa me)
                                 (* 2 (bb me)))))]

    (is (= 1 (raw-svr t-1 :aa)))
    (is (= 3 (raw-svr t-1 :bb)))
    (is (= 7 (raw-svr t-1 :cc)))

    (is (= 1 (aa t-1)))
    (is (= 3 (bb t-1)))
    (is (= 7 (cc t-1)))
    (is (= 7 @cco))
    (is (= (cc t-1) @cco))))

#_
(deftest add-x-to-y-a-using-are
  (are [x y] (= 5 (add x y))
       2 3
       1 4
       3 2))

