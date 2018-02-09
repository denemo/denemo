;;;; InsertDirectivesAroundSelection
;;;If the selection comprises two non-music objects it cuts them to the clipboard and  memorises their types
;;;;;if not it inserts inserts the first and second objects in the clipboard around the current selection, checking for their types.
      
(define-once InsertDirectivesAroundSelection::start #f)
(define-once InsertDirectivesAroundSelection::end #f)
                    
(let
   ((selection-end #f)
    (within-measure #f)
    (measure-num #f)
    (start-direc #f)
    (end-direc #f)
    (message #f))
(define (help)
        (d-WarningDialog (string-append (if message message "\n") "\n" (_ "First execute this command with the selection set to two directives, clef changes, tuplet markers etc (which will then be copied to the clipboard).")
        "\n" (_  "Then select music to be bracketed by these two objects and re-execute the command")))
        (set! message #f))
            
    (d-PushPosition)
    (d-GoToSelectionStart)
    (set! measure-num (d-GetMeasure))
    (if (MoveToEndOfSelection)
        (begin
            (set! within-measure (= measure-num (d-GetMeasure)))
            (if  (not (Music?))
                (begin
                    (if  (Directive?)
                        (set! InsertDirectivesAroundSelection::end (d-DirectiveGetForTag-standalone))
                        (set! InsertDirectivesAroundSelection::end (d-GetType)))
                    (d-MoveCursorLeft)
                    (if (not (Music?))
                        (begin ;;; we are initializing the system
                            
                            (if  (Directive?)
                                (set! InsertDirectivesAroundSelection::start (d-DirectiveGetForTag-standalone))
                                (set! InsertDirectivesAroundSelection::start (d-GetType)))
                            (d-WarningDialog (string-append (_ "The pair of objects") " " InsertDirectivesAroundSelection::start
                                " " "and" " " InsertDirectivesAroundSelection::end " " "will now be cut to the clipboard. Invoking this command in future will bracket selected music with those two objects."))
                            (d-Cut) ;;;put the two objects on the clipboard
                            )
                        (begin
                            (set! message (_ "Notes or rests cannot be used."))
                            (set! InsertDirectivesAroundSelection::start #f)
                            (set! InsertDirectivesAroundSelection::end #f))))
                ;;;; we have a selection not ending on a directive: surround with the clipboard objects
                (begin
                    (set! selection-end (GetPosition))
                    
                    (if (and InsertDirectivesAroundSelection::start InsertDirectivesAroundSelection::end)
                        (begin 
                            (d-GoToSelectionStart)
                            (if (and (d-PutClipObj 0 0) (d-MoveCursorLeft) (or (equal? InsertDirectivesAroundSelection::start (d-GetType)) (equal? (d-DirectiveGetForTag-standalone) InsertDirectivesAroundSelection::start)))
                                (begin
                                    (apply d-GoToPosition selection-end)
                                    (d-MoveCursorRight)
                                    (if within-measure (d-MoveCursorRight));; because we have inserted one clip obj in this measure
                                    (if (and (d-PutClipObj 0 1) (d-MoveCursorLeft) (or (equal? InsertDirectivesAroundSelection::end (d-GetType))(equal? (d-DirectiveGetForTag-standalone) InsertDirectivesAroundSelection::end)))
                                        (set! message #f) ;;; success
                                        (begin
                                            (set! message (_ "Clipboard is messed up, edit by hand"))
                                            (set! InsertDirectivesAroundSelection::start #f)
                                            (set! InsertDirectivesAroundSelection::end #f))))
                                (begin
                                    (d-DeleteObject)
                                    (set! InsertDirectivesAroundSelection::start #f)
                                    (set! InsertDirectivesAroundSelection::end #f)
                                    (set! message (_ "Clipboard has changed and does not hold a pair of objects to bracket the selection. Restart the command.")))))
                        (begin
                            (set! message (_ "No pair of objects available - start again, selecting a pair of objects to be used.")))))))
        (begin
            (set! message (_ "No Selection"))))
    (if message (help))
    (d-PopPosition))