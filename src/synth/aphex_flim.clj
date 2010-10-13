(ns synth.aphex-flim
  (:use overtone.live))

;; Original instruments and patterns graciously borrowed from scsynth code here:
;; http://github.com/rukano/scprivatepool/tree/master/projects/aphex_flim/

(def metro (metronome 120))

(defn ranged [in lo hi & [bi-polar?]]
  (with-ugens
    (if bi-polar?
      (let [mul (* 0.5 (- hi lo))
            add (+ mul lo)]
        (mul-add in mul add))
      (let [mul (- hi lo)
            add lo]
        (mul-add in mul add)))))

(defsynth ping [out 0 freq 440 attack 0.001 release 1 amp 0.1 pan 0]
  (let [snd (f-sin-osc freq)
        env (env-gen (perc attack release 1 -20) :action :free)]
    (offset-out out (pan2 (* snd env) pan))))

(defsynth peng [out 0 freq 440 attack 0.0001 release 1 amp 0.1
                pan 0 mod-freq 111 detune 2 rq 0.8]
  (let [mod (sin-osc mod-freq)
        snd (apply + (saw [freq (* freq (rrand 1.45 1.55))]))
        snd (* mod snd)
        filt (rhpf snd (/ freq 2) (* rq 2))
        filt (rlpf filt (* freq 2) (/ rq 2))
        env (env-gen (perc attack release 1 -12) :action :free)]
    (offset-out out (pan2 (* filt env) pan))))

;(peng)

(defsynth kick [out 0 ffreq 80 attack 0 release 2 amp 0.1 pan 0]
  (let [snd (apply + (sin-osc [ffreq (* 1.01 ffreq) (* ffreq 1.03)
                      (* ffreq 1.06) (* ffreq 1.1) 0 0.5]))
        snd (+ snd (pink-noise))
        snd (reduce (fn [mem v]
                      (rlpf mem (* ffreq v) (* 0.1 v)))
                    snd (range 1 6))
        snd (+ snd (lpf (white-noise) (* ffreq 6)))
        env (env-gen (perc attack release 1 -50) :action :free)]
    (offset-out out (pan2 (* snd amp env) 0))))

(defsynth bass [out 0 freq 100 attack 0.1 release 2 amp 0.1 pan 0]
  (let [starts [freq (* freq 1.5) (* freq 2) (* freq 3)]
        ends (map #(/ % 4) starts)
        snd (apply + (sin-osc (+ 0.5 (x-line:kr starts ends (/ release 2)))))
        filt (rlpf snd (* freq (/ 2 3)) 0.1)
        env (env-gen (perc attack release 2 -10) :action :free)]
    (offset-out out (pan2 (* amp env filt) pan))))

(defsynth snare [out 0 freq 100 rq 0.5 attack 0 release 0.5 amp 0.1 pan 0]
  (let [snd (white-noise)
        filt (hpf snd freq)
        filt (+ filt (hpf filt (* freq 1.345)))
        filt (+ filt (hpf filt (* freq 2.456)))
        filt (+ filt (hpf filt (* freq 4.345)))
        snd (lpf filt 2000)
        snd (* 0.2 (rhpf snd (* 2 freq) rq))
        snd (* snd (env-gen (perc attack release 1 -10) :action :free))]
    (offset-out out (pan2 (* snd amp) pan))))

(defsynth hi-hat [out 0 ffreq 200 rq 0.5
                  attack 0 release 0.025 amp 0.1
                  pan 0]
  (let [snd (white-noise)
        snd (hpf snd ffreq)
        snd (rhpf snd (* ffreq 2) rq)
        snd (* snd (env-gen (perc attack release 1 -10) :action :free))]
    (offset-out out (pan2 (* 2 snd amp)))))

(defn panner [beat]
  (at (metro beat)
      (kick))
  (at (metro (+ 0.5 beat))
      (ping 0 220 0.001 1 0.1 (Math/sin (* 0.7 beat))))
  (if (even? beat)
    (at (metro (+ 0.75 beat))
        (ping 0 440 0.001 1 0.1 (Math/sin (* 0.7 beat)))))

  (when (zero? (mod beat 7))
    (if (zero? (rand-int 3))
      (at (metro (+ beat 0.70))
          (bass 0 220 :release 0.13)))
    (at (metro (+ beat 0.994))
        (bass 0 110 :release 4)))

  (when (odd? (mod beat 4))
    (at (metro beat)
        (snare)))

  (when (= 0 (mod beat 4))
    (at (metro (+ 0.5 beat))
        (kick)))

  (when (= 0 (mod beat 6))
    (dotimes [i 4]
      (at (metro (+ (* i 0.2) beat))
          (ping 0 (* 220 (inc i)) 0.001 1 0.1 (Math/sin (* i 1.2 beat))))))
  (apply-at #'panner (metro (inc beat)) (inc beat)))

(panner (metro))


