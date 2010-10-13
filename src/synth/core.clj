(ns synth.core
  (:use overtone.live))

(definst pad [f 440 attack 0.001 release 0.2] 
         (* (env-gen (perc attack release) :action :free) 
            (+ (sin-osc (/ f 2)) 
               (saw [f (* 1.01 f)]))))

(defn play-pad []
  (doseq [i [440 440 220 330 440]] 
  (foo i) 
  (Thread/sleep 250)))

(
