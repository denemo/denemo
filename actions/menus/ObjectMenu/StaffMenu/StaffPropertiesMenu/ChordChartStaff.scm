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
            (ToggleDirective "voice" "prefix" (cons tag (_ "Chord Chart")) (string-append "\\new ChordNames \\with {

         \\consists \"Bar_engraver\"
         \\consists \"Script_engraver\"
         \\consists \"Text_engraver\"
         \\consists \"Time_signature_engraver\"
         \\consists \"Multi_measure_rest_engraver\"
         \\override ChordName.font-size=#" size "
         \\override ChordName.extra-offset = #'(0 . -2)
         \\override BarLine.bar-extent = #'(-2 . 2)
         \\override BarLine #'hair-thickness = #6
       
         
         
        \\numericTimeSignature 
    }
    ")  DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_GRAPHIC)
            
             
            (d-DirectivePut-score-display "CustomBarline" (_ "Custom Barlines"))
            (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX))
            (d-DirectivePut-score-prefix tag "\n\\defineBarLine \"|\" #'(\"|\" \"|\" \"|\")\n")
            
            (d-CustomBarline (format #f "'~s" (list (_ "Single") "|" "|" "|" "")))
            (d-CustomBarline (format #f "'~s" (list (_ "Double") "|" "||" "||" "")))
            (d-CustomBarline (format #f "'~s" (list (_ "RepeatStart")  "[|:"  "[|:" "||" "")))           
            (d-CustomBarline (format #f "'~s" (list (_ "RepeatEnd") "|" ":|]"  ":|]"  "")))
            (d-Set0)
            (d-ScoreIndent 0)
            (d-RaggedLast)
            (d-BarNumberingInterval 10000) ;; no bar numbers
            (d-DirectivePut-layout-postfix tag "\\set noChordSymbol = \\markup \\smaller \\bold  \"/\"")
            (ToggleDirective "clef" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND)
            (ToggleDirective "keysig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX)
            (ToggleDirective "timesig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_GRAPHIC)
            (d-DirectivePut-timesig-display tag "4/4"))))
