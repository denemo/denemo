;;ToggleSuspension
(let ((tag "ToggleSuspension"))
                        (ChordAnnotation tag "\\suspension"   ToggleSuspension::params   "suspension"))
(if (not (d-Directive-score? "Allow\nsuspension"))
    (d-LilyPondDefinition (cons "suspension" (string-append "^\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "suspension.eps\""   "}" ))))
 
