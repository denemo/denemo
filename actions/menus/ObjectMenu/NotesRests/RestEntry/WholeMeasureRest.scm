;;;WholeMeasureRest
(if (or (d-IsAppending) (d-Directive-chord? DenemoWholeMeasureRestTag))
 (let ((timesig #f))
    (set! timesig (GetPrevailingTimeSig)) 
    (if (not (d-Directive-chord? DenemoWholeMeasureRestTag))
        (d-InsertWholeRest)
        (TimedNotice (_ "Duration of whole measure rest has been re-calculated") 5000))
        
    (d-SetDurationInTicks (* 1536 (GetPrevailingTimeSig #t)))
    (d-DirectivePut-chord-graphic DenemoWholeMeasureRestTag "\n\x20")
    (d-DirectivePut-chord-gx DenemoWholeMeasureRestTag 60)
    (d-DirectivePut-chord-display DenemoWholeMeasureRestTag (string-append (_ "Rest ") timesig))
    (d-DirectivePut-chord-tx DenemoWholeMeasureRestTag 55)
    (d-DirectivePut-chord-ty DenemoWholeMeasureRestTag 15)
    (d-DirectivePut-chord-minpixels DenemoWholeMeasureRestTag 100)
    (d-DirectivePut-chord-override DenemoWholeMeasureRestTag (logior DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
    
    (if (and (equal? DenemoWholeMeasureRestParams "edit") (d-GetNonprinting))
            (set! DenemoWholeMeasureRestParams 'nonprinting)) ;;preserve non-printing status on right-click

    (if (eq? DenemoWholeMeasureRestParams 'nonprinting)
            (d-DirectivePut-chord-postfix DenemoWholeMeasureRestTag (string-append "s1*" timesig))
            (d-DirectivePut-chord-postfix DenemoWholeMeasureRestTag (string-append "R1*" timesig)))
    (d-SetObjectDisplayWidth 100)
    (d-RefreshDisplay)
    (d-SetSaved #f)))
