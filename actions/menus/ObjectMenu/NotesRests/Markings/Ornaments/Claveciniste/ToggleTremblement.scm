;;ToggleTremblement
(let ((tag "ToggleTremblement"))
                        (ChordOrnament tag "\\tremblement"   ToggleTremblement::params   "tremblement"))
(if (not (d-Directive-score? "Allow\ntremblement"))
    (begin
        (d-LilyPondDefinition (cons "tremblement" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "tremblement.eps\""   "}" )))
        (d-DirectivePut-score-data "Allow\ntremblement" (string-append "(list \"tremblement\" \"" (scheme-escape (string-append DENEMO_GRAPHICS_DIR "tremblement.eps")) "\" \"2\")"))))
 

