;;;WholeMeasureRest
(if (or (d-IsAppending) (d-Directive-chord? "WholeMeasureRest"))
 (let ((timesig #f))
    (set! timesig (d-InsertTimeSig "query=timesigname")) 
    (if (not (d-Directive-chord? "WholeMeasureRest"))
        (d-InsertWholeRest)
        (d-InfoDialog (_ "Duration of whole measure rest has been re-calculated")))
        
        
    (d-SetDurationInTicks (* 1536 (GetPrevailingTimeSig #t)))
    (d-DirectivePut-chord-graphic "WholeMeasureRest" "\n\x20")
    (d-DirectivePut-chord-gx "WholeMeasureRest" 60)
    (d-DirectivePut-chord-display "WholeMeasureRest" "1")
    (d-DirectivePut-chord-tx "WholeMeasureRest" 76)
    (d-DirectivePut-chord-ty"WholeMeasureRest" 15)
    (d-DirectivePut-chord-minpixels"WholeMeasureRest" 100)
    (d-DirectivePut-chord-override "WholeMeasureRest" (logior DENEMO_OVERRIDE_LILYPOND DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
    
    (if (and (equal? WholeMeasureRest::params "edit") (d-GetNonprinting))
            (set! WholeMeasureRest::params 'nonprinting)) ;;preserve non-printing status on right-click

    (if (eq? WholeMeasureRest::params 'nonprinting)
            (d-DirectivePut-chord-postfix "WholeMeasureRest" (string-append "s1*" timesig))
            (d-DirectivePut-chord-postfix "WholeMeasureRest" (string-append "R1*" timesig)))
    (d-SetObjectDisplayWidth 100)
    (d-SetSaved #f)))
