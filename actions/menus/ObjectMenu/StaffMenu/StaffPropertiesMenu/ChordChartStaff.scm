;;;;;ChordChartStaff
(let ((tag "ChordChartStaff"))
    (if (and ChordStaff::params (d-Directive-voice? tag))
        (begin (d-InfoDialog "Use the Staffs/Voices Chord Chart command to turn off the typestting of chord names for this staff"))
        (let ((size "8"))
            (if (d-Directive-staff? tag)
                (d-DirectiveDelete-staff tag)
                (begin
                    (set! size (d-GetUserInput (_ "Chord Chart Staff") (_ "Give size of chord symbols required") size))
                    (ToggleDirective "staff" "prefix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))
                    (ToggleDirective "staff" "postfix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))))
            (d-DirectiveDelete-staff "InstrumentName")
            (ToggleDirective "voice" "prefix" tag (string-append "\\new ChordNames \\with {
        \\override ChordName.font-size=#" size "
        \\override ChordName.extra-offset = #'(0 . -2)
         \\override BarLine.bar-extent = #'(-2 . 2)
         \\consists \"Bar_engraver\"
         \\consists \"Script_engraver\"
         \\consists \"Text_engraver\"
         \\consists \"Time_signature_engraver\"
         \\consists \"Multi_measure_rest_engraver\"
        \\numericTimeSignature 
    }
    ")  DENEMO_OVERRIDE_LILYPOND)
            (d-DirectivePut-layout-postfix tag "\\set noChordSymbol = \"\\\\\"")
            (ToggleDirective "clef" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND)
            (if (d-Directive-clef? tag) (d-DirectivePut-clef-display tag "     CC"))
            (ToggleDirective "keysig" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX)
            (ToggleDirective "timesig" "postfix" tag "\n" DENEMO_OVERRIDE_LILYPOND))))
