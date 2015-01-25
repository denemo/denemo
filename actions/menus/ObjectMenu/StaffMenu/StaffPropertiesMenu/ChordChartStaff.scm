;;;;;ChordChartStaff
(let ((tag "ChordChartStaff") (params ChordChartStaff::params)(choice #f))

    (define (create-chart compact)
     (if (and ChordStaff::params (d-Directive-voice? tag))
                (begin (d-InfoDialog "Use the Staffs/Voices Chord Chart command to turn off the typesetting of chord names for this staff"))
                (let ()
                    (if (d-Directive-staff? tag)
                        (begin
                            (d-DirectiveDelete-timesig tag)
                            (d-DirectiveDelete-keysig tag)
                            (d-DirectiveDelete-clef tag)
                        
                            (d-DirectiveDelete-staff tag)
                            (d-DirectiveDelete-voice tag))
                        (begin
                                    (while (d-DirectiveGetForTag-staff)
                                       (d-DirectiveDelete-staff (d-DirectiveGetForTag-staff))) 
                                    (while (d-DirectiveGetForTag-voice)
                                       (d-DirectiveDelete-voice (d-DirectiveGetForTag-voice)))
                                    (ToggleDirective "staff" "prefix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))
                                    (ToggleDirective "staff" "postfix" tag ""(logior  DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX))))
                                    (ToggleDirective "voice" "prefix" (cons tag (_ "Chord Chart")) (string-append "\\new ChordNames \\with {"
(if compact
"chordNameExceptions = #(sequential-music-to-chord-exceptions CompactChordSymbols #t)\n"
"")
  
"                \\consists \"Bar_engraver\"
                
                 \\consists \"Script_engraver\"
                 \\consists \"Text_engraver\""
                 (if compact "" "\\consists \"Time_signature_engraver\"\n") "
                 \\consists \"Multi_measure_rest_engraver\"
                 " 
 (if compact                
                 "
                 \\override ChordName.Y-extent = ##f
                 \\override ChordName.extra-spacing-width=#'(+inf.0 . -inf.0)
                 \\override ChordName.extra-offset = #'(0 . -2.5)
                 \\override BarLine.bar-extent = #'(-3.5 . 4.2)
                 \\override BarLine #'hair-thickness = #1.2 "
                 "
                 \\override ChordName.extra-offset = #'(0 . -2.5)
                 \\override BarLine.bar-extent = #'(-3.5 . 3.5)
                 \\override BarLine #'hair-thickness = #1.2 "
                 )"    
            }
                                ")  DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_GRAPHIC)
                                
                                 
                                (d-DirectivePut-score-display "CustomBarline" (_ "Custom Barlines"))
                                (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX))
                                (d-DirectivePut-score-prefix tag "\n\\defineBarLine \"|\" #'(\"|\" \"|\" \"|\")\n")
                                
                                (d-CustomBarline (format #f "'~s" (list (_ "Single") "|" "|" "|" "")))
                                (d-CustomBarline (format #f "'~s" (list (_ "Double") "||" "||" "||" "")))
                                (d-CustomBarline (format #f "'~s" (list (_ "RepeatStart")  "[|:"  "[|:" "||" "")))           
                                (d-CustomBarline (format #f "'~s" (list (_ "RepeatEnd") "|" ":|]"  ":|]"  "")))                                
                                (d-CustomBarline (format #f "'~s" (list (_ "RepeatEndFirstTime") "" ":|]"  ":|]"  "")))

                                (d-Set0)
                                (d-ScoreIndent 0)
                                (if compact
                                    (begin
                                        (d-MeasuresPerLine)
                                        (d-SetPageSize "(cons \"5\" \"8.1\")")
                                        (d-SetFontSize "7.2")))
                                (let ((stag "CompactChordChartSettings"))

                                        (d-DirectivePut-paper-postfix stag  "
system-system-spacing =
#'((basic-distance . 2)
(minimum-distance . 2)
(padding . 2))\n")
                                    (set! stag "LeftMargin")
                                    (d-DirectivePut-paper-data stag 0.07)
                                    (d-DirectivePut-paper-postfix stag "left-margin=0.07\\cm\n")
                                    (set! stag "RightMargin")
                                    (d-DirectivePut-paper-data stag 0.07)
                                    (d-DirectivePut-paper-postfix stag "right-margin=0.07\\cm\n")
                                )

                                (d-DirectivePut-score-override "ChordNamer" (logior DENEMO_OVERRIDE_AFFIX))
                                
                                (d-DirectivePut-layout-postfix "ChordNamer" (string-append "
\\context {
        \\Score\n"
        (if compact
            "chordCompactScale = #'(2.5 . 3.0)\n"
            "")
        "\\remove \"Bar_number_engraver\"
        } "))
                                (d-DirectivePut-layout-postfix tag "\\set noChordSymbol = \\markup \\fontsize #6 \\bold  \"/\"")
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
                                            ;(d-LineBreak #t)
                                            (d-MoveToBeginning)
                                            (if compact
                                                (begin
                                                    (d-InitialTimeSig)
                                                    (d-ChordChartTimeSignature)))
                                            (d-MoveCursorRight)
                                            (d-MoveCursorRight))
                                            
                                        (if compact
                                            (d-DirectivePut-score-display "CompactChordChart" (_ "Compact Chord Chart Marker")))
                                        ;;;(d-BarNumberingInterval 10000) ;; no bar numbers
                                        (ToggleDirective "clef" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND)
                                        (ToggleDirective "keysig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_AFFIX)
                                        (ToggleDirective "timesig" "postfix" (cons tag "") "\n" DENEMO_OVERRIDE_LILYPOND  DENEMO_OVERRIDE_GRAPHIC)
                                        (d-DirectivePut-timesig-display tag (d-GetPrevailingTimesig)))))))
                               
  (if params
        (set! choice params)
        (set! choice (RadioBoxMenu (cons (_ "Compact Chord Chart") 'compact) (cons (_ "Paper Chord Chart") 'new) (cons (_ "Convert Staff") 'convert) (cons (_ "Customizable Chord Chart") 'custom) (cons (_ "Cancel") 'cancel))))
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
