;;ToggleTremblementAppuye
(let ((tag "ToggleTremblementAppuye")(filename (scheme-escape (string-append DENEMO_GRAPHICS_DIR "tremblement_appuye.eps"))))
                        (ChordOrnament tag "\\tremblement_appuye"   ToggleTremblementAppuye::params   "tremblement_appuye")
(if (not (d-Directive-score? "Allow\nTremblement_appuye"))
    (begin
        (d-LilyPondDefinition (cons "tremblement_appuye" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" filename "\""   "}" )))
        (d-DirectivePut-score-data "Allow\ntremblement_appuye" (string-append "(list \"tremblement_appuye\" \"" filename "\" \"2\")")))))
