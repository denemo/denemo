%%% grob-inspector.ily derived from Mathieu Demange's code


#(define (grob-origin grob)
       (let* ((cause (ly:grob-property grob 'cause))
              (music-origin (if (ly:stream-event? cause)
                                (ly:event-property cause 'origin))))
         (if (ly:input-location? music-origin)
              (let ((location (ly:input-file-line-char-column music-origin)))
                (format #f " ~a:~a"
                           (cadr location)
                           (caddr location)))
            " Unknown" )))




#(define (override-color-for-all-grobs color)
  (lambda (context)
   (let loop ((x all-grob-descriptions))
    (if (not (null? x))
     (let ((grob-name (caar x)))
      (ly:context-pushpop-property context grob-name 'color color)
      (loop (cdr x)))))))

#(define (format-moment moment)
   (exact->inexact
    (/ (ly:moment-main-numerator moment)
      (ly:moment-main-denominator moment))))

#(define (grob-moment-when grob class)
   (display "Class: ")
   (display class)
   (display " When: ")
   (display (format-moment (grob::when grob)))
   (display "\n")
   
   )

#(define (format-id grob class) 
   (define location (grob-origin grob))
   (string-concatenate 
    (list 
     "class:"
     class
     ";"
     "location"
      location
      ";"
     "data-moment:"
     (ly:format "~a" (format-moment (grob::when grob)))
     ";"
     "data-measure:"
     (ly:format "~a" (car (grob::rhythmic-location grob)))
     )))


#(define (accidental-id grob)

   (format-id grob '"ly grob accidental")

   )
#(define (accidentalcautionary-id grob)

   (format-id grob '"ly grob accidentalcautionary")

   )
#(define (accidentalplacement-id grob)

   (format-id grob '"ly grob accidentalplacement")

   )
#(define (accidentalsuggestion-id grob)

   (format-id grob '"ly grob accidentalsuggestion")

   )
#(define (ambitus-id grob)

   (format-id grob '"ly grob ambitus")

   )
#(define (ambitusaccidental-id grob)

   (format-id grob '"ly grob ambitusaccidental")

   )
#(define (ambitusline-id grob)

   (format-id grob '"ly grob ambitusline")

   )
#(define (ambitusnotehead-id grob)

   (format-id grob '"ly grob ambitusnotehead")

   )
#(define (arpeggio-id grob)

   (format-id grob '"ly grob arpeggio")

   )
#(define (balloontextitem-id grob)

   (format-id grob '"ly grob balloontextitem")

   )
#(define (barline-id grob)

   (format-id grob '"ly grob barline")

   )
#(define (barnumber-id grob)

   (format-id grob '"ly grob barnumber")

   )
#(define (bassfigure-id grob)

   (format-id grob '"ly grob bassfigure")

   )
#(define (bassfigurealignment-id grob)

   (format-id grob '"ly grob bassfigurealignment")

   )
#(define (bassfigurealignmentpositioning-id grob)

   (format-id grob '"ly grob bassfigurealignmentpositioning")

   )
#(define (bassfigurebracket-id grob)

   (format-id grob '"ly grob bassfigurebracket")

   )
#(define (bassfigurecontinuation-id grob)

   (format-id grob '"ly grob bassfigurecontinuation")

   )
#(define (bassfigureline-id grob)

   (format-id grob '"ly grob bassfigureline")

   )
#(define (beam-id grob)

   (format-id grob '"ly grob beam")

   )
#(define (bendafter-id grob)

   (format-id grob '"ly grob bendafter")

   )
#(define (breakaligngroup-id grob)

   (format-id grob '"ly grob breakaligngroup")

   )
#(define (breakalignment-id grob)

   (format-id grob '"ly grob breakalignment")

   )
#(define (breathingsign-id grob)

   (format-id grob '"ly grob breathingsign")

   )
#(define (chordname-id grob)

   (format-id grob '"ly grob chordname")

   )
#(define (clef-id grob)

   (format-id grob '"ly grob clef")

   )
#(define (clefmodifier-id grob)

   (format-id grob '"ly grob clefmodifier")

   )
#(define (clusterspanner-id grob)

   (format-id grob '"ly grob clusterspanner")

   )
#(define (clusterspannerbeacon-id grob)

   (format-id grob '"ly grob clusterspannerbeacon")

   )
#(define (combinetextscript-id grob)

   (format-id grob '"ly grob combinetextscript")

   )
#(define (cueclef-id grob)

   (format-id grob '"ly grob cueclef")

   )
#(define (cueendclef-id grob)

   (format-id grob '"ly grob cueendclef")

   )
#(define (custos-id grob)

   (format-id grob '"ly grob custos")

   )
#(define (dotcolumn-id grob)

   (format-id grob '"ly grob dotcolumn")

   )
#(define (dots-id grob)

   (format-id grob '"ly grob dots")

   )
#(define (doublepercentrepeat-id grob)

   (format-id grob '"ly grob doublepercentrepeat")

   )
#(define (doublepercentrepeatcounter-id grob)

   (format-id grob '"ly grob doublepercentrepeatcounter")

   )
#(define (doublerepeatslash-id grob)

   (format-id grob '"ly grob doublerepeatslash")

   )
#(define (dynamiclinespanner-id grob)

   (format-id grob '"ly grob dynamiclinespanner")

   )
#(define (dynamictext-id grob)

   (format-id grob '"ly grob dynamictext")

   )
#(define (dynamictextspanner-id grob)

   (format-id grob '"ly grob dynamictextspanner")

   )
#(define (episema-id grob)

   (format-id grob '"ly grob episema")

   )
#(define (fingering-id grob)

   (format-id grob '"ly grob fingering")

   )
#(define (fingeringcolumn-id grob)

   (format-id grob '"ly grob fingeringcolumn")

   )
#(define (flag-id grob)

   (format-id grob '"ly grob flag")

   )
#(define (footnoteitem-id grob)

   (format-id grob '"ly grob footnoteitem")

   )
#(define (footnotespanner-id grob)

   (format-id grob '"ly grob footnotespanner")

   )
#(define (fretboard-id grob)

   (format-id grob '"ly grob fretboard")

   )
#(define (glissando-id grob)

   (format-id grob '"ly grob glissando")

   )
#(define (gracespacing-id grob)

   (format-id grob '"ly grob gracespacing")

   )
#(define (gridline-id grob)

   (format-id grob '"ly grob gridline")

   )
#(define (gridpoint-id grob)

   (format-id grob '"ly grob gridpoint")

   )
#(define (hairpin-id grob)

   (format-id grob '"ly grob hairpin")

   )
#(define (horizontalbracket-id grob)

   (format-id grob '"ly grob horizontalbracket")

   )
#(define (instrumentname-id grob)

   (format-id grob '"ly grob instrumentname")

   )
#(define (instrumentswitch-id grob)

   (format-id grob '"ly grob instrumentswitch")

   )
#(define (keycancellation-id grob)

   (format-id grob '"ly grob keycancellation")

   )
#(define (keysignature-id grob)

   (format-id grob '"ly grob keysignature")

   )
#(define (kievanligature-id grob)

   (format-id grob '"ly grob kievanligature")

   )
#(define (laissezvibrertie-id grob)

   (format-id grob '"ly grob laissezvibrertie")

   )
#(define (laissezvibrertiecolumn-id grob)

   (format-id grob '"ly grob laissezvibrertiecolumn")

   )
#(define (ledgerlinespanner-id grob)

   (format-id grob '"ly grob ledgerlinespanner")

   )
#(define (leftedge-id grob)

   (format-id grob '"ly grob leftedge")

   )
#(define (ligaturebracket-id grob)

   (format-id grob '"ly grob ligaturebracket")

   )
#(define (lyricextender-id grob)

   (format-id grob '"ly grob lyricextender")

   )
#(define (lyrichyphen-id grob)

   (format-id grob '"ly grob lyrichyphen")

   )
#(define (lyricspace-id grob)

   (format-id grob '"ly grob lyricspace")

   )
#(define (lyrictext-id grob)

   (format-id grob '"ly grob lyrictext")

   )
#(define (measurecounter-id grob)

   (format-id grob '"ly grob measurecounter")

   )
#(define (measuregrouping-id grob)

   (format-id grob '"ly grob measuregrouping")

   )
#(define (melodyitem-id grob)

   (format-id grob '"ly grob melodyitem")

   )
#(define (mensuralligature-id grob)

   (format-id grob '"ly grob mensuralligature")

   )

#(define (metronomemark-id grob) 
   
   (string-concatenate 
    (list 
     "class:ly grob metronomemark;"
     "data-moment:"
     (ly:format "~a" (format-moment (grob::when grob)))
     ";"
     "data-metronome-count:"
     (ly:format "~a" (ly:event-property (ly:grob-property grob 'cause) 'metronome-count))
     ";"
     "data-tempo-unit:"
     (ly:format "~a" (ly:duration->string (ly:event-property (ly:grob-property grob 'cause) 'tempo-unit))))))

#(define (multimeasurerest-id grob)

   (format-id grob '"ly grob multimeasurerest")

   )
#(define (multimeasurerestnumber-id grob)

   (format-id grob '"ly grob multimeasurerestnumber")

   )
#(define (multimeasureresttext-id grob)

   (format-id grob '"ly grob multimeasureresttext")

   )
#(define (nonmusicalpapercolumn-id grob)

   (format-id grob '"ly grob nonmusicalpapercolumn")

   )
#(define (notecollision-id grob)

   (format-id grob '"ly grob notecollision")

   )
#(define (notecolumn-id grob)

   (format-id grob '"ly grob notecolumn")

   )
#(define (notehead-id grob)

   (format-id grob '"ly grob notehead")

   )
#(define (notename-id grob)

   (format-id grob '"ly grob notename")

   )
#(define (notespacing-id grob)

   (format-id grob '"ly grob notespacing")

   )
#(define (ottavabracket-id grob)

   (format-id grob '"ly grob ottavabracket")

   )
#(define (papercolumn-id grob)

   (format-id grob '"ly grob papercolumn")

   )
#(define (parenthesesitem-id grob)

   (format-id grob '"ly grob parenthesesitem")

   )
#(define (percentrepeat-id grob)

   (format-id grob '"ly grob percentrepeat")

   )
#(define (percentrepeatcounter-id grob)

   (format-id grob '"ly grob percentrepeatcounter")

   )
#(define (phrasingslur-id grob)

   (format-id grob '"ly grob phrasingslur")

   )
#(define (pianopedalbracket-id grob)

   (format-id grob '"ly grob pianopedalbracket")

   )
#(define (rehearsalmark-id grob)

   (format-id grob '"ly grob rehearsalmark")

   )
#(define (repeatslash-id grob)

   (format-id grob '"ly grob repeatslash")

   )
#(define (repeattie-id grob)

   (format-id grob '"ly grob repeattie")

   )
#(define (repeattiecolumn-id grob)

   (format-id grob '"ly grob repeattiecolumn")

   )
#(define (rest-id grob)

   (format-id grob '"ly grob rest")

   )
#(define (restcollision-id grob)

   (format-id grob '"ly grob restcollision")

   )
#(define (script-id grob)

   (format-id grob '"ly grob script")

   )
#(define (scriptcolumn-id grob)

   (format-id grob '"ly grob scriptcolumn")

   )
#(define (scriptrow-id grob)

   (format-id grob '"ly grob scriptrow")

   )
#(define (slur-id grob)

   (format-id grob '"ly grob slur")

   )
#(define (sostenutopedal-id grob)

   (format-id grob '"ly grob sostenutopedal")

   )
#(define (sostenutopedallinespanner-id grob)

   (format-id grob '"ly grob sostenutopedallinespanner")

   )
#(define (spacingspanner-id grob)

   (format-id grob '"ly grob spacingspanner")

   )
#(define (spanbar-id grob)

   (format-id grob '"ly grob spanbar")

   )
#(define (spanbarstub-id grob)

   (format-id grob '"ly grob spanbarstub")

   )
#(define (staffgrouper-id grob)

   (format-id grob '"ly grob staffgrouper")

   )
#(define (staffspacing-id grob)

   (format-id grob '"ly grob staffspacing")

   )
#(define (staffsymbol-id grob)

   (format-id grob '"ly grob staffsymbol")

   )
#(define (stanzanumber-id grob)

   (format-id grob '"ly grob stanzanumber")

   )
#(define (stem-id grob)

   (format-id grob '"ly grob stem")

   )
#(define (stemstub-id grob)

   (format-id grob '"ly grob stemstub")

   )
#(define (stemtremolo-id grob)

   (format-id grob '"ly grob stemtremolo")

   )
#(define (stringnumber-id grob)

   (format-id grob '"ly grob stringnumber")

   )
#(define (strokefinger-id grob)

   (format-id grob '"ly grob strokefinger")

   )
#(define (sustainpedal-id grob)

   (format-id grob '"ly grob sustainpedal")

   )
#(define (sustainpedallinespanner-id grob)

   (format-id grob '"ly grob sustainpedallinespanner")

   )
#(define (system-id grob)

   (format-id grob '"ly grob system")

   )
#(define (systemstartbar-id grob)

   (format-id grob '"ly grob systemstartbar")

   )
#(define (systemstartbrace-id grob)

   (format-id grob '"ly grob systemstartbrace")

   )
#(define (systemstartbracket-id grob)

   (format-id grob '"ly grob systemstartbracket")

   )
#(define (systemstartsquare-id grob)

   (format-id grob '"ly grob systemstartsquare")

   )
#(define (tabnotehead-id grob)

   (format-id grob '"ly grob tabnotehead")

   )
#(define (textscript-id grob)

   (format-id grob '"ly grob textscript")

   )
#(define (textspanner-id grob)

   (format-id grob '"ly grob textspanner")

   )
#(define (tie-id grob)

   (format-id grob '"ly grob tie")

   )
#(define (tiecolumn-id grob)

   (format-id grob '"ly grob tiecolumn")

   )
#(define (timesignature-id grob)

   (format-id grob '"ly grob timesignature")

   )
#(define (trillpitchaccidental-id grob)

   (format-id grob '"ly grob trillpitchaccidental")

   )
#(define (trillpitchgroup-id grob)

   (format-id grob '"ly grob trillpitchgroup")

   )
#(define (trillpitchhead-id grob)

   (format-id grob '"ly grob trillpitchhead")

   )
#(define (trillspanner-id grob)

   (format-id grob '"ly grob trillspanner")

   )
#(define (tupletbracket-id grob)

   (format-id grob '"ly grob tupletbracket")

   )
#(define (tupletnumber-id grob)

   (format-id grob '"ly grob tupletnumber")

   )
#(define (unacordapedal-id grob)

   (format-id grob '"ly grob unacordapedal")

   )
#(define (unacordapedallinespanner-id grob)

   (format-id grob '"ly grob unacordapedallinespanner")

   )
#(define (vaticanaligature-id grob)

   (format-id grob '"ly grob vaticanaligature")

   )
#(define (verticalalignment-id grob)

   (format-id grob '"ly grob verticalalignment")

   )
#(define (verticalaxisgroup-id grob)

   (format-id grob '"ly grob verticalaxisgroup")

   )
#(define (voicefollower-id grob)

   (format-id grob '"ly grob voicefollower")

   )
#(define (voltabracket-id grob)

   (format-id grob '"ly grob voltabracket")

   )
#(define (voltabracketspanner-id grob)

   (format-id grob '"ly grob voltabracketspanner")

   )








#(define (acc-id grob)
   
   (display (ly:event-property (ly:grob-property (ly:grob-property grob 'cause) 'cause) 'origin))
   
   )

#(define (display-when grob)
   
   (display grob)
   (display "\n")
   (display (grob::when grob))
   (display "-----\n")
   
   )

\layout {
 
  \override Score.Accidental.id = #accidental-id
  \override Score.AccidentalCautionary.id = #accidentalcautionary-id
  \override Score.AccidentalPlacement.id = #accidentalplacement-id
  \override Score.AccidentalSuggestion.id = #accidentalsuggestion-id
  \override Score.Ambitus.id = #ambitus-id
  \override Score.AmbitusAccidental.id = #ambitusaccidental-id
  \override Score.AmbitusLine.id = #ambitusline-id
  \override Score.AmbitusNoteHead.id = #ambitusnotehead-id
  \override Score.Arpeggio.id = #arpeggio-id
  \override Score.BalloonTextItem.id = #balloontextitem-id
  \override Score.BarLine.id = #barline-id
  \override Score.BarNumber.id = #barnumber-id
  \override Score.BassFigure.id = #bassfigure-id
  \override Score.BassFigureAlignment.id = #bassfigurealignment-id
  \override Score.BassFigureAlignmentPositioning.id = #bassfigurealignmentpositioning-id
  \override Score.BassFigureBracket.id = #bassfigurebracket-id
  \override Score.BassFigureContinuation.id = #bassfigurecontinuation-id
  \override Score.BassFigureLine.id = #bassfigureline-id
  \override Score.Beam.id = #beam-id
  \override Score.BendAfter.id = #bendafter-id
  \override Score.BreakAlignGroup.id = #breakaligngroup-id
  \override Score.BreakAlignment.id = #breakalignment-id
  \override Score.BreathingSign.id = #breathingsign-id
  \override Score.ChordName.id = #chordname-id
  \override Score.Clef.id = #clef-id
  \override Score.ClefModifier.id = #clefmodifier-id
  \override Score.ClusterSpanner.id = #clusterspanner-id
  \override Score.ClusterSpannerBeacon.id = #clusterspannerbeacon-id
  \override Score.CombineTextScript.id = #combinetextscript-id
  \override Score.CueClef.id = #cueclef-id
  \override Score.CueEndClef.id = #cueendclef-id
  \override Score.Custos.id = #custos-id
  \override Score.DotColumn.id = #dotcolumn-id
  \override Score.Dots.id = #dots-id
  \override Score.DoublePercentRepeat.id = #doublepercentrepeat-id
  \override Score.DoublePercentRepeatCounter.id = #doublepercentrepeatcounter-id
  \override Score.DoubleRepeatSlash.id = #doublerepeatslash-id
  \override Score.DynamicLineSpanner.id = #dynamiclinespanner-id
  \override Score.DynamicText.id = #dynamictext-id
  \override Score.DynamicTextSpanner.id = #dynamictextspanner-id
  \override Score.Episema.id = #episema-id
  \override Score.Fingering.id = #fingering-id
  \override Score.FingeringColumn.id = #fingeringcolumn-id
  \override Score.Flag.id = #flag-id
  \override Score.FootnoteItem.id = #footnoteitem-id
  \override Score.FootnoteSpanner.id = #footnotespanner-id
  \override Score.FretBoard.id = #fretboard-id
  \override Score.Glissando.id = #glissando-id
  \override Score.GraceSpacing.id = #gracespacing-id
  \override Score.GridLine.id = #gridline-id
  \override Score.GridPoint.id = #gridpoint-id
  \override Score.Hairpin.id = #hairpin-id
  \override Score.HorizontalBracket.id = #horizontalbracket-id
  \override Score.InstrumentName.id = #instrumentname-id
  \override Score.InstrumentSwitch.id = #instrumentswitch-id
  \override Score.KeyCancellation.id = #keycancellation-id
  \override Score.KeySignature.id = #keysignature-id
  \override Score.KievanLigature.id = #kievanligature-id
  \override Score.LaissezVibrerTie.id = #laissezvibrertie-id
  \override Score.LaissezVibrerTieColumn.id = #laissezvibrertiecolumn-id
  \override Score.LedgerLineSpanner.id = #ledgerlinespanner-id
  \override Score.LeftEdge.id = #leftedge-id
  \override Score.LigatureBracket.id = #ligaturebracket-id
  \override Score.LyricExtender.id = #lyricextender-id
  \override Score.LyricHyphen.id = #lyrichyphen-id
  \override Score.LyricSpace.id = #lyricspace-id
  \override Score.LyricText.id = #lyrictext-id
  \override Score.MeasureCounter.id = #measurecounter-id
  \override Score.MeasureGrouping.id = #measuregrouping-id
  \override Score.MelodyItem.id = #melodyitem-id
  \override Score.MensuralLigature.id = #mensuralligature-id
  \override Score.MetronomeMark.id = #metronomemark-id
  \override Score.MultiMeasureRest.id = #multimeasurerest-id
  \override Score.MultiMeasureRestNumber.id = #multimeasurerestnumber-id
  \override Score.MultiMeasureRestText.id = #multimeasureresttext-id
  \override Score.NonMusicalPaperColumn.id = #nonmusicalpapercolumn-id
  \override Score.NoteCollision.id = #notecollision-id
  \override Score.NoteColumn.id = #notecolumn-id
  \override Score.NoteHead.id = #notehead-id
  \override Score.NoteName.id = #notename-id
  \override Score.NoteSpacing.id = #notespacing-id
  \override Score.OttavaBracket.id = #ottavabracket-id
  \override Score.PaperColumn.id = #papercolumn-id
  \override Score.ParenthesesItem.id = #parenthesesitem-id
  \override Score.PercentRepeat.id = #percentrepeat-id
  \override Score.PercentRepeatCounter.id = #percentrepeatcounter-id
  \override Score.PhrasingSlur.id = #phrasingslur-id
  \override Score.PianoPedalBracket.id = #pianopedalbracket-id
  \override Score.RehearsalMark.id = #rehearsalmark-id
  \override Score.RepeatSlash.id = #repeatslash-id
  \override Score.RepeatTie.id = #repeattie-id
  \override Score.RepeatTieColumn.id = #repeattiecolumn-id
  \override Score.Rest.id = #rest-id
  \override Score.RestCollision.id = #restcollision-id
  \override Score.Script.id = #script-id
  \override Score.ScriptColumn.id = #scriptcolumn-id
  \override Score.ScriptRow.id = #scriptrow-id
  \override Score.Slur.id = #slur-id
  \override Score.SostenutoPedal.id = #sostenutopedal-id
  \override Score.SostenutoPedalLineSpanner.id = #sostenutopedallinespanner-id
  \override Score.SpacingSpanner.id = #spacingspanner-id
  \override Score.SpanBar.id = #spanbar-id
  \override Score.SpanBarStub.id = #spanbarstub-id
  \override Score.StaffGrouper.id = #staffgrouper-id
  \override Score.StaffSpacing.id = #staffspacing-id
  \override Score.StaffSymbol.id = #staffsymbol-id
  \override Score.StanzaNumber.id = #stanzanumber-id
  \override Score.Stem.id = #stem-id
  \override Score.StemStub.id = #stemstub-id
  \override Score.StemTremolo.id = #stemtremolo-id
  \override Score.StringNumber.id = #stringnumber-id
  \override Score.StrokeFinger.id = #strokefinger-id
  \override Score.SustainPedal.id = #sustainpedal-id
  \override Score.SustainPedalLineSpanner.id = #sustainpedallinespanner-id
  \override Score.System.id = #system-id
  \override Score.SystemStartBar.id = #systemstartbar-id
  \override Score.SystemStartBrace.id = #systemstartbrace-id
  \override Score.SystemStartBracket.id = #systemstartbracket-id
  \override Score.SystemStartSquare.id = #systemstartsquare-id
  \override Score.TabNoteHead.id = #tabnotehead-id
  \override Score.TextScript.id = #textscript-id
  \override Score.TextSpanner.id = #textspanner-id
  \override Score.Tie.id = #tie-id
  \override Score.TieColumn.id = #tiecolumn-id
  \override Score.TimeSignature.id = #timesignature-id
  \override Score.TrillPitchAccidental.id = #trillpitchaccidental-id
  \override Score.TrillPitchGroup.id = #trillpitchgroup-id
  \override Score.TrillPitchHead.id = #trillpitchhead-id
  \override Score.TrillSpanner.id = #trillspanner-id
  \override Score.TupletBracket.id = #tupletbracket-id
  \override Score.TupletNumber.id = #tupletnumber-id
  \override Score.UnaCordaPedal.id = #unacordapedal-id
  \override Score.UnaCordaPedalLineSpanner.id = #unacordapedallinespanner-id
  \override Score.VaticanaLigature.id = #vaticanaligature-id
  \override Score.VerticalAlignment.id = #verticalalignment-id
  \override Score.VerticalAxisGroup.id = #verticalaxisgroup-id
  \override Score.VoiceFollower.id = #voicefollower-id
  \override Score.VoltaBracket.id = #voltabracket-id
  \override Score.VoltaBracketSpanner.id = #voltabracketspanner-id
 
 
}

#(open-file  "events.txt" "w")
pageBreak = {}
