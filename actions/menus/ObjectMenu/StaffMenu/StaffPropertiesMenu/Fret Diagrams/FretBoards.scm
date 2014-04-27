;;;FretBoards
(let ((tag "FretBoards"))
	(ToggleDirective "staff" "postfix" tag " \\new FretBoards <<\n"  DENEMO_OVERRIDE_LILYPOND)
	(ToggleDirective "voice" "postfix" tag "\n" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
	(ToggleDirective "clef" "postfix" tag "{} \n"  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND)))

