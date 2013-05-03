;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;ChordStaff
(let ((tag "ChordStaff"))
(if (and ChordStaff::params (d-Directive-voice? tag))
	(begin (d-InfoDialog "Use the Staffs/Voices Chord Names command to turn off the typestting of chord names for this staff"))
	(begin
		(if (d-Directive-staff? tag)
			(d-DirectiveDelete-staff tag)
			(begin
				(ToggleDirective "staff" "prefix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))
				(ToggleDirective "staff" "postfix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))))
		(d-DirectiveDelete-staff "InstrumentName")
		(ToggleDirective "voice" "prefix" tag "\\new ChordNames \n"  DENEMO_OVERRIDE_LILYPOND)
		(ToggleDirective "clef" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND)
		(if (d-Directive-clef? tag) (d-DirectivePut-clef-display tag "     X"))
		(ToggleDirective "keysig" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX)
		(ToggleDirective "timesig" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND))))
