
(ns tiltontec.its-alive.hellow-world
  (:require [clojure.test :refer :all]
            [tiltontec.its-alive.utility :refer :all] 
            [tiltontec.its-alive.cell-types :refer :all :as cty]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.evaluate :refer :all]
            [tiltontec.its-alive.cells :refer :all]
            ))

(println :---------alons-------------------)

(cells-init)

(let [call (c-in nil)
      loc (c-in :away)
      resp (c?+ [:obs (fn-obs (trx :obs-resp new))]
                (trx :running!!!!!!!)
                (when-let [call (c-get call)]
                 (case call
                   :knock-knock (case (c-get loc)
                                  :home "Hello, world! "
                                  :away "No one home!"
                                  "Help!"))))]
  (c-reset! loc :home)                  
  (c-reset! loc :away)
  ;;(c-get resp) ;; wakes up resp cell and call dependency
  ;;(c-reset! call :knock-knock)
  ;;(c-reset! loc :home)                  
  )


;; (let [call (c-in nil :ephemeral? true)
;;       act (c-in nil :ephemeral? true)
;;       loc (c?+ [:obs (fn-obs (when new (trx :honey-im new)))]
;;                (case (c-get act)
;;                  :leave :away
;;                  :return :home
;;                  :lost))
;;       resp (c?+ [:obs (fn-obs (when new (trx :resp-is new)))]
;;                 (when-let [call (c-get call)]
;;                   (case call
;;                     :knock-knock (case (c-get loc)
;;                                    :home (str "Hello, world! " (rand-int 32000))
;;                                    :away "No one home!"
;;                                    "Help!")
;;                     call)))]
;;   (c-get resp)
;;   (c-reset! call :knock-knock)
;;   (c-reset! act :return)
;;   (c-reset! call :knock-knock)
;;   (c-reset! act :leave)
;;   (c-reset! call :knock-knock)  
;;   )
  
               

(println :-------fini---------------------)
