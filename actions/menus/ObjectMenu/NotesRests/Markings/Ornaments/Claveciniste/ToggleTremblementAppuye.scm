;;ToggleTremblementAppuye
(let ((tag "ToggleTremblementAppuye"))
                        (ChordOrnament tag "\\tremblement_appuye"   ToggleTremblementAppuye::params   "tremblement_appuye"))
(if (not (d-Directive-score? "Allow\nTremblement_appuye"))
    (begin
        (d-LilyPondDefinition (cons "tremblement_appuye" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "tremblement_appuye.eps\""   "}" )))
        (d-DirectivePut-score-data "Allow\ntremblement_appuye" (string-append "(list \"tremblement_appuye\" \"" (scheme-escape (string-append DENEMO_GRAPHICS_DIR "tremblement_appuye.eps")) "\" \"2\")"))))
