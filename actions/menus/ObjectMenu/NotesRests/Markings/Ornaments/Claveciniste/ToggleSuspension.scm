;;ToggleSuspension
(let ((tag "ToggleSuspension"))
    (ChordOrnament tag "\\suspension"   ToggleSuspension::params   "suspension"))
(if (not (d-Directive-score? "Allow\nsuspension"))
    (begin
        (d-LilyPondDefinition (cons "suspension" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "suspension.eps\""   "}" )))
        (d-DirectivePut-score-data "Allow\nsuspension" (string-append "(list \"suspension\" \"" (scheme-escape (string-append DENEMO_GRAPHICS_DIR "suspension.eps")) "\" \"2\")"))))
 
