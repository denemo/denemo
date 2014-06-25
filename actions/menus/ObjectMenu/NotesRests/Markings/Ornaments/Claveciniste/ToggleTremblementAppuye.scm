;;ToggleTremblementAppuye
(let ((tag "ToggleTremblementAppuye"))
                        (ChordAnnotation tag "\\tremblement_appuye"   ToggleTremblementAppuye::params   "tremblement_appuye"))
(if (not (d-Directive-score? "Allow\nTremblement_appuye"))
    (d-LilyPondDefinition (cons "tremblement_appuye" (string-append "^\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "tremblement_appuye.eps\""   "}" ))))
 
