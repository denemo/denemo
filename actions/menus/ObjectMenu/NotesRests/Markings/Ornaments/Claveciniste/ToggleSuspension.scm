;;ToggleSuspension
(let ((tag "ToggleSuspension"))
                        (ChordAnnotation tag "\\suspension"   ToggleSuspension::params   "suspension"))
(if (not (d-Directive-score? "AllowSuspension"))
    (d-LilyPondDefinition (cons "suspension" (string-append "^\\markup {\\epsfile #X #2 #\"" DENEMO_GLYPHS_DIR "suspension.eps\""   "}" ))))
 