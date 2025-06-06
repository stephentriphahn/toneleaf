(ns toneleaf.core
  (:use [overtone.live]))

(defn vary [val pct]
  (let [range (* val pct)]
    (+ val (- (* 2 range) (* range (rand)))))) ;; ±pct of val

;; Simulated plant data
(def plant-state {:moisture 70
                  :light 400
                  :temp 22
                  :hour 14})

(defn plant->music [state]
  (let [{:keys [moisture light temp hour]} state
        freq         (+ 200 (* moisture 2))
        tempo        (double (- 120 (* (/ (- 100 moisture) 100) 60))) ;; 60–120 bpm
        scale        (if (> light 500) :major :minor-pentatonic)
        synth        (cond
                       (< temp 10) :saw
                       (< temp 20) :sine
                       :else       :triangle)
        reverb       (/ moisture 100.0)
        cutoff       (+ 500 (* temp 30))
        env          {:attack (/ light 1000.0)
                      :decay  0.8
                      :sustain (/ moisture 100.0)
                      :release (/ (- 24 hour) 24.0)}]
    {:freq freq
     :tempo tempo
     :scale scale
     :synth synth
     :reverb reverb
     :cutoff-freq cutoff
     :env env}))

;; Define a simple ambient synth
(definst plant-voice
  [freq 440
   amp 0.3
   attack 0.5
   decay 0.8
   sustain 0.6
   release 1.2
   cutoff 1000
   reverb-mix 0.3
   wave-index 0 ; 0 = sine, 1 = triangle, 2 = saw
   ]
  (let [env    (env-gen (adsr attack decay sustain release) :action FREE)
        sine     (sin-osc freq)
        triangle (lf-tri freq)
        saw      (saw freq)
        osc      (select wave-index [sine triangle saw])
        filtered (rlpf osc cutoff 0.3)
        sig     (* env amp filtered)]
    (free-verb sig reverb-mix 0.9)))

(defn synth->index [synth]
  (case synth
    :sine 0
    :triangle 1
    :saw 2
    0)) ; default to sine

(defn plant-loop-harmony
  [plant-state & {:keys [interval-ms] :or {interval-ms 3000}}]

  (let [base-params (plant->music plant-state)
        {:keys [freq synth cutoff-freq reverb env]} base-params
        wave-index (synth->index synth)
        {:keys [attack decay sustain release]} env

        ;; Create 3 note intervals (in Hz)
        freqs [(vary freq 0.02)
               (vary (* freq 1.25) 0.02) ; major third above
               (vary (* freq 1.5) 0.02)] ; perfect fifth above
        ]

    ;; Play each note in harmony
    (doseq [f freqs]
      (plant-voice
       :freq f
       :cutoff (vary cutoff-freq 0.05)
       :reverb-mix (vary reverb 0.1)
       :attack (vary attack 0.2)
       :decay (vary decay 0.2)
       :sustain (vary sustain 0.2)
       :release (vary release 0.2)
       :wave-index wave-index))

    ;; Schedule next cycle
    (apply-at (+ (now) interval-ms)
              #'plant-loop-harmony [plant-state :interval-ms interval-ms])))

(comment
  ;; Fire up SuperCollider before running!
  ;;(boot-external-server) use seems to do this automatically

  (plant-loop-harmony plant-state)
  (stop))
