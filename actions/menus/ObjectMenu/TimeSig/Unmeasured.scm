;Unmeasured
(let ((tag "Unmeasured"))
            (d-InitialTimeSig "256/1")
            (d-DirectivePut-timesig-postfix tag "\\cadenzaOn ")
             (d-DirectivePut-timesig-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
            (d-DirectivePut-timesig-graphic tag "\n \n")
            (d-SetSaved #f))