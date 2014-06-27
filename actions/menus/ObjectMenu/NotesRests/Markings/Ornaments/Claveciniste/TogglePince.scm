;;TogglePince
(let ((tag "TogglePince"))
                        (ChordOrnament tag "\\pince"   TogglePince::params   "pince"))
(if (not (d-Directive-score? "Allow\npince"))
    (begin
    (d-LilyPondDefinition (cons "pince" (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #2 #\"" DENEMO_GRAPHICS_DIR "pince.eps\""   "}" )))
    (d-DirectivePut-score-data "Allow\npince" (string-append "(list \"pince\" \"" (scheme-escape (string-append DENEMO_GRAPHICS_DIR "pince.eps")) "\" \"2\")"))))
 
