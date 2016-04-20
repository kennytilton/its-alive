(ns tiltontec.its-alive.model
  (:require [tiltontec.its-alive.utility :refer :all]
            [tiltontec.its-alive.globals :refer :all]
            [tiltontec.its-alive.cell-types :refer :all as cty]
            [tiltontec.its-alive.observer :refer :all]
            [tiltontec.its-alive.integrity :refer :all]
            [tiltontec.its-alive.cells :refer :all]))

(comment
  (defmd box
    xl xr yt yb
    color
    [area (c? (* (- xr? xl?)
                 (- yt? yb?)))]
    [open (c-in true)]
    [material :cardboard :cell false]))

(comment
  (progn
   (derive ia-types ::box ::model)
   (defn mk-box [& kvs]
     (let [me (ref nil)]
       (ref-set me
                (merge {
                        :me me
                        :state :nascent
                        :slots [:xl :xr :yt :yb :color :area :open :material]
                        :cells #{} ;; cull from kvs
                        :xl nil :xr nil :yt nil :yb nil
                        :color nil
                        :area (c? (* (- (xr?) (xl?))
                                     (- (yt?) (yb?))))
                        :open (c-in true)
                        :material :cardboard
                        }
                       (apply hash-map kvs)))
       (md-slot-initialize me)
       (with-meta me
         {:type ::box})))))

#_
(let [slots [:xl :xr :yt :yb :color :area :open :material]
      uargs (hash-map :xl 42 :area (c? (* 3.1416 2 3))
                      :name :bob 
                      :color (c-in "#f00"))]
  (select-keys uargs slots))
  
  


             
