;;;VoiceSetting
        (let ( (choice (d-GetOption (string-append (_ "Voice 1") stop  (_ "Voice 2") stop (_ "Voice 3") stop (_ "Voice 4") stop (_ "Automatic Voice") stop))))
          (if choice
            (cond ((string=? choice (_ "Voice 1"))
                    (d-VoicePreset1))
                  ((string=? choice (_ "Voice 2"))
                    (d-VoicePreset2))
                  ((string=? choice (_ "Voice 3"))
                    (d-VoicePreset3))
                  ((string=? choice (_ "Voice 4"))
                    (d-VoicePreset4))
                  ((string=? choice (_ "Automatic Voice"))
                  (d-VoicePresetAutomatic)))))
        