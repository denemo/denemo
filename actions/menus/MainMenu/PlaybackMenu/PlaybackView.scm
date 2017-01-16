;;;PlaybackView
(if (d-ContinuousTypesetting)
  (d-WarningDialog (_ "Please turn continuous typsetting off first"))
  (let ((params PlaybackView::params)(tag "Temp")(tag2 "Temp2")(pagebreak (d-Directive-header? "MovementPageBreak"))(booktitles #f)(data #f)(width "20")(height "100") (part #f)(changecount (d-Changecount))(saved (d-GetSaved)))
    (define (no-tempo-at-start)
        (define no-tempo #t)
        (d-PushPosition)
        (d-MoveToBeginning)
      
        (let loop ()
          (if (Music?)
            (set! no-tempo #t)
            (if (d-Directive-standalone? "MetronomeMark")
                (set! no-tempo #f)
                (if (d-NextObjectInMeasure)
                    (loop)
                    (set! no-tempo #t)))))
        (d-PopPosition)
        no-tempo)
;;;procedure starts here
;(disp "starting " changecount "\n\n")
    (set! part (eq? params 'part))
    (if (string? params) (set! params (eval-string params)))
    (if (list? params)
        (begin
            (set! part (list-ref params 0))
            (set! width (list-ref params 1))
            (set! height (list-ref params 2))))
     (disp "Have " part " and "  (d-GetMovement) " and layout "   (d-GetLayoutName) "\n")    
    (if (> (d-GetMovement) 1)
        (let ((layout (string-append "Movement " (number->string (d-GetMovement)))))
            (if (not (equal? layout (d-GetLayoutName)))
                (d-WarningDialog (_ "You should typeset this movement first in the Print View, otherwise positioning may not work.")))))
(d-IncreaseGuard)
    (set! booktitles (DenemoHasBookTitles))
    (if booktitles
        (DenemoHideBookTitles))
    (if pagebreak
        (begin
            (set! pagebreak (d-DirectiveGet-header-override "MovementPageBreak"))
            (d-DirectivePut-header-override "MovementPageBreak" DENEMO_OVERRIDE_HIDDEN)))
    (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
    (d-DirectivePut-score-prefix tag "\n\\include \"live-score.ily\"\n")
    (if (no-tempo-at-start)
      (d-DirectivePut-voice-postfix tag (string-append " \\set Score.tempoHideNote = ##t \\tempo 4=" (number->string (d-MovementTempo)) " ")))
    (d-DirectivePut-score-prefix tag2 "\n\\header { tagline = #f }\n")
    (d-DirectivePut-score-postfix tag2 "  \\applyContext #(override-color-for-all-grobs (x11-color 'black)) ")
    (d-DirectivePut-movementcontrol-postfix tag "\n\\midi {}\n")
    (d-DirectivePut-movementcontrol-override tag DENEMO_OVERRIDE_AFFIX)
    (d-DirectivePut-layout-postfix tag "%{For Performance View%}")
    (d-DirectivePut-paper-postfix tag (string-append "
    ragged-bottom = ##t
    #(set! paper-alist (cons '(\"custom-size\" . (cons (* " width " cm) (* " height " cm))) paper-alist))
    #(set-paper-size \"custom-size\")"))
    (d-SetSaved saved)
    (d-Changecount changecount)
    (d-DisplayTypesetSvg (/ (string->number  (d-ScoreProperties "query=fontsize"))18.0) part)
     
    (if pagebreak
        (d-DirectivePut-header-override "MovementPageBreak" pagebreak))   
    (d-DirectiveDelete-movementcontrol tag)
    (d-DirectiveDelete-paper tag)
    (d-DirectiveDelete-score tag)
    (d-DirectivePut-score-prefix tag "\n%\\include \"live-score.ily\"\n") ;;to keep the same line numbers we don't delete this line but insert it commented it out
    (d-DirectivePut-score-override tag DENEMO_OVERRIDE_AFFIX)
    (d-DirectiveDelete-voice tag)
    (d-DirectiveDelete-score tag2)
    (d-DirectivePut-score-prefix tag2 "\n%\\header { tagline = #f }\n") ;;to keep the same line numbers we don't delete this line but comment it out
    (if booktitles
        (DenemoUseBookTitles))
    (d-SetSaved saved);(disp "Resetting " changecount "\n\n")
    (d-Changecount changecount)
(d-DecreaseGuard)    
    ))
