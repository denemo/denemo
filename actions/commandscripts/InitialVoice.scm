;;;InitialVoiceSetting
        (let ( (choice (d-GetOption (string-append (_ "Voice 1") stop  (_ "Voice 2") stop (_ "Voice 3") stop (_ "Voice 4") stop (_ "Automatic Voice") stop))))
          (if choice
            (cond ((string=? choice (_ "Voice 1"))
                    (d-InitialVoiceOne))
                  ((string=? choice (_ "Voice 2"))
                    (d-InitialVoiceTwo))
                  ((string=? choice (_ "Voice 3"))
                    (d-InitialVoiceThree))
                  ((string=? choice (_ "Voice 4"))
                    (d-InitialVoiceFour))
                  ((string=? choice (_ "Automatic Voice"))
                  (d-InitialVoiceAuto)))))
        