;;TogglePince
(let ((tag "TogglePince"))
                        (ChordAnnotation tag "\\pince"   TogglePince::params   "pince"))
(if (not (d-Directive-score? "Allow\npince"))
    (d-LilyPondDefinition (cons "pince" (string-append "^\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "pince.eps\""   "}" ))))
 
