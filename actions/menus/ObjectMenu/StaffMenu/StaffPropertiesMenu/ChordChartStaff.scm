;;;;;ChordChartStaff
(let ((tag "ChordChartStaff")
        (choice (RadioBoxMenu (cons (_ "Compact Chord Chart") 'compact) (cons (_ "Paper Chord Chart") 'new) (cons (_ "Convert Staff") 'convert) (cons (_ "Customizable Chord Chart") 'custom) (cons (_ "Cancel") 'cancel))))
    (define (create-chart compact)
     (if (and ChordStaff::params (d-Directive-voice? tag))
                (begin (d-InfoDialog "Use the Staffs/Voices Chord Chart command to turn off the typesetting of chord names for this staff"))
                (let ((size (if compact "12" "8")))
                    (if (d-Directive-staff? tag)
                        (begin
                            (d-DirectiveDelete-timesig tag)
                            (d-DirectiveDelete-keysig tag)
                            (d-DirectiveDelete-clef tag)
                        
                            (d-DirectiveDelete-staff tag)
                            (d-DirectiveDelete-voice tag))
                        (begin
                            (if (not compact)
                                (set! size (d-GetUserInput (_ "Chord Chart Staff") (_ "Give size of chord symbols required") size)))
                            (if size
                                (begin
                                    (while (d-DirectiveGetForTag-staff)
                                       (d-DirectiveDelete-staff (d-DirectiveGetForTag-staff))) 
                                    (while (d-DirectiveGetForTag-voice)
                                       (d-DirectiveDelete-voice (d-DirectiveGetForTag-voice)))
                                    (ToggleDirective "staff" "prefix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))
                                    (ToggleDirective "staff" "postfix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))))
                                    (ToggleDirective "voice" "prefix" (cons tag (_ "Chord Chart")) (string-append "\\new ChordNames \\with {"
(if compact
"chordNameExceptions = #(append
  (sequential-music-to-chord-exceptions CompactChordSymbols #t)
  ignatzekExceptions)
"
"")
  
"                 \\consists \"Bar_engraver\"
                 \\consists \"Script_engraver\"
                 \\consists \"Text_engraver\""
                 (if compact "" "\\consists \"Time_signature_engraver\"\n") "
                 \\consists \"Multi_measure_rest_engraver\"
                 \\override ChordName.font-size=#" 
                 size      
 (if compact                
                 "
                 \\override ChordName.Y-extent = ##f
                 \\override ChordName.extra-spacing-width=#'(+inf.0 . -inf.0)
                 \\override ChordName.extra-offset = #'(0 . -2)
                 \\override BarLine.bar-extent = #'(-4 . 6)
                 \\override BarLine #'hair-thickness = #4 "
                 "
                 \\override ChordName.extra-offset = #'(0 . -2)
                 \\override BarLine.bar-extent = #'(-2 . 2)
                 \\override BarLine #'hair-thickness = #4 "
                 )"    
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
                                (d-CustomBarline (format #f "'~s" (list (_ "RepeatEndFirstTime") "" ":|]"  ":|]"  "")))

                                (d-Set0)
                                (d-ScoreIndent 0)
                                (let ((stag "CompactChordChartSettings"))
                                        (d-DirectivePut-paper-postfix stag  "
 system-system-spacing =
#'((basic-distance . 2)
(minimum-distance . 2)
(padding . 2))\n"))

                                (d-DirectivePut-score-override "ChordNamer" (logior DENEMO_OVERRIDE_AFFIX))
                                (d-DirectivePut-score-prefix "ChordNamer" "
#(define (conditional-string-downcase str condition)
  (if condition
      (string-downcase str)
      str))

#(define (denemo-chord-name->pop-markup pitch lowercase?)
  (let* ((alt (ly:pitch-alteration pitch)))
  (make-line-markup
    (list
      (make-bold-markup (make-scale-markup '(0.5 . 1) (make-simple-markup 
       (conditional-string-downcase
        (vector-ref #(\"C\" \"D\" \"E\" \"F\" \"G\" \"A\" \"B\") (ly:pitch-notename
pitch))
        lowercase?))))
      (if (= alt 0)
    (make-hspace-markup 1)
    (make-line-markup
            (list
              (make-hspace-markup 0.1)
              (make-fontsize-markup -6 (make-raise-markup 5
                 (alteration->text-accidental-markup alt))))))))))
#(define (denemo-chord-inv-name->pop-markup pitch lowercase?)
  (let* ((alt (ly:pitch-alteration pitch)))
  (make-line-markup
    (list
      (make-bold-markup (make-scale-markup '(0.5 . 1) (make-simple-markup 
       (conditional-string-downcase
        (vector-ref #(\"C\" \"D\" \"E\" \"F\" \"G\" \"A\" \"B\") (ly:pitch-notename
pitch))
        lowercase?))))
      (if (= alt 0)
        (make-line-markup (list empty-markup))
          (make-line-markup
            (list
              (make-hspace-markup 0.1)
              (make-fontsize-markup -4 
                 (alteration->text-accidental-markup alt)))))))))                                     
                                ")
                               (d-DirectivePut-layout-postfix "ChordNamer" "
\\context {
        \\Score
        chordRootNamer = #denemo-chord-name->pop-markup
        } ")
                                (d-DirectivePut-layout-postfix tag "\\set noChordSymbol = \\markup \\smaller \\bold  \"/\"")
                                 (if (d-Directive-staff? tag)
                                    (begin
                                        (d-ProportionalNotation 4)
                                        (let ((btag "TextScript"))
                                            (d-Directive-standalone btag)
                                            (d-DirectivePut-standalone-postfix btag "\\BarSingle")
                                            (d-DirectivePut-standalone-display btag "BarSingle")
                                            (d-DirectivePut-standalone-minpixels btag 30)
                                            (d-AppendMeasure)
                                            (d-AppendMeasure)
                                            (d-AppendMeasure)
                                            (d-AppendMeasure)
                                            (d-MoveToEnd)
                                            (d-LineBreak #t)
                                            (d-MoveToBeginning)
                                            (if compact
                                                (d-ChordChartTimeSignature))
                                            (d-MoveCursorRight)
                                            (d-MoveCursorRight))
                                            
                                        (if compact
                                            (d-DirectivePut-score-display "CompactChordChart" (_ "Compact Chord Chart Marker")))
                                        (d-BarNumberingInterval 10000) ;; no bar numbers
                                        (ToggleDirective "clef" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND)
                                        (ToggleDirective "keysig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX)
                                        (ToggleDirective "timesig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_GRAPHIC)
                                        (d-DirectivePut-timesig-display tag "4/4"))))))))

    (case choice
    
        ((cancel)
            (d-InfoDialog (_ "Cancelled")))
        ((convert)
            (create-chart #f))
        ((compact)
            (if (d-New)
                (begin
                    (create-chart #t)
                    (d-LilyPondInclude "compact-chord-symbols.ily"))))
        ((custom)
            (if (d-New)
                (d-OpenTemplate "filename=ChordChart.denemo")))
        ((new) (if (d-New)
            (create-chart #f)
            (d-InfoDialog (_ "Cancelled"))))))
