;;ToggleSuspension
(let ((tag "ToggleSuspension")(filename (scheme-escape (string-append DENEMO_GRAPHICS_DIR "suspension.eps"))))
    (ChordOrnament tag "\\suspension"   ToggleSuspension::params   "suspension")
(if (not (d-Directive-score? "Allow\nsuspension"))
    (begin
        (d-LilyPondDefinition (cons "suspension" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" filename "\""   "}" )))
        (d-DirectivePut-score-data "Allow\nsuspension" (string-append "(list \"suspension\" \"" filename  "\" \"2\")")))))
 
