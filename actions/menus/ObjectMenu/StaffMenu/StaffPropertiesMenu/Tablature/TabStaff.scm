;;;TabStaff
(let ((tag "TabStaff"))
	(ToggleDirective "staff" "postfix" tag " \\new TabStaff <<\n"  DENEMO_OVERRIDE_LILYPOND)
	(ToggleDirective "voice" "postfix" tag "\n" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
	(ToggleDirective "clef" "postfix" tag "{} \n"  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND)))

