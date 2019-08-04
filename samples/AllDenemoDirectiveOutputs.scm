(d-InsertA)
(d-AddD)
(d-DirectivePut-chord-prefix "test" "%{chord prefix%}\n")
(d-DirectivePut-chord-postfix "test" "%{chord postfix%}\n")

(d-Change0)
(d-InsertB)
(d-AddD)
(d-DirectivePut-chord-prefix "testwithaffix" "%{chord prefix with affix override set%}\n")
(d-DirectivePut-chord-postfix "testwithaffix" "%{chord postfix with affix override set%}\n")
(d-DirectivePut-chord-override "testwithaffix" DENEMO_OVERRIDE_AFFIX)

(d-Change1)
(d-InsertC)
(d-AddD)
(d-DirectivePut-chord-prefix "testwithaffixandlilyoverride" "%{chord prefix with lily and affix override set%}\n") ;; does not need to output in LilyPond
(d-DirectivePut-chord-postfix "testwithaffixandlilyoverride" "%{chord postfix with lily and affix override set%}\n")
(d-DirectivePut-chord-override "testwithaffixandlilyoverride" (logior DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_AFFIX))

(d-Change2)
(d-InsertD)
(d-DirectivePut-note-prefix "test" "%{note prefix%}\n")
(d-DirectivePut-note-postfix "test" "%{note postfix%}\n")

(d-DirectivePut-staff-prefix "test" "%{staff prefix%}\n")
(d-DirectivePut-staff-postfix "test" "%{staff postfix%}\n")

(d-DirectivePut-voice-prefix "test" "%{voice prefix%}\n")
(d-DirectivePut-voice-postfix "test" "%{voice postfix%}\n")

(d-DirectivePut-score-prefix "test" "%{score prefix%}\n")
(d-DirectivePut-score-postfix "test" "%{score postfix%}\n")

(d-DirectivePut-score-override "scorewithaffix" DENEMO_OVERRIDE_AFFIX)
(d-DirectivePut-score-prefix "scorewithaffix" "%{score prefix with affix%}\n")
(d-DirectivePut-score-postfix "scorewithaffix" "%{score postfix with affix%}\n")

(d-DirectivePut-clef-prefix "test" "%{clef prefix%}\n")
(d-DirectivePut-clef-postfix "test" "%{clef postfix%}\n")

(d-DirectivePut-scoreheader-prefix "test" "%{scoreheader prefix%}\n")
(d-DirectivePut-scoreheader-postfix "test" "%{scoreheader postfix%}\n")

(d-DirectivePut-movementcontrol-prefix "test" "%{movementcontrol prefix%}\n")
(d-DirectivePut-movementcontrol-postfix "test" "%{movementcontrol postfix%}\n")

(d-DirectivePut-movementcontrol-prefix "movementcontrolwithaffix" "%{movementcontrol prefix with affix%}\n")
(d-DirectivePut-movementcontrol-postfix "movementcontrolwithaffix" "%{movementcontrol postfix with affix%}\n")
(d-DirectivePut-movementcontrol-override "movementcontrolwithaffix" DENEMO_OVERRIDE_AFFIX)

;;(d-DirectivePut-movementcontrol-override "test" DENEMO_OVERRIDE_LILYPOND)

(d-DirectivePut-header-prefix "test" "%{header prefix%}\n")
(d-DirectivePut-header-postfix "test" "%{header postfix%}\n")

(d-DirectivePut-paper-prefix "test" "%{paper prefix%}\n")
(d-DirectivePut-paper-postfix "test" "%{paper postfix%}\n")

(d-DirectivePut-layout-prefix "test" "%{layout prefix%}\n")
(d-DirectivePut-layout-postfix "test" "%{layout postfix%}\n")



(d-DirectivePut-standalone-prefix "test" "%{standalone prefix%}")
(d-DirectivePut-standalone-postfix "test" "%{standalone postfix%}")

(d-RefreshDisplay)
