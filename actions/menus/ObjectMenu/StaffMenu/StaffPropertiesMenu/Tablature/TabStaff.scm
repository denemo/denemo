(ToggleDirective "staff" "postfix" "TabStaff" " \\new TabStaff <<\n"  DENEMO_OVERRIDE_LILYPOND)
(ToggleDirective "voice" "postfix" "TabStaff" "\n" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
(ToggleDirective "clef" "postfix" "TabStaff" "{} \n"  (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
