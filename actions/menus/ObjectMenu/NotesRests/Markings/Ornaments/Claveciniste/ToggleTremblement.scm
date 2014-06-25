;;ToggleTremblement
(let ((tag "ToggleTremblement"))
                        (ChordAnnotation tag "\\tremblement"   ToggleTremblement::params   "tremblement"))
(if (not (d-Directive-score? "Allow\ntremblement"))
    (d-LilyPondDefinition (cons "tremblement" (string-append "^\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "tremblement.eps\""   "}" ))))
 
