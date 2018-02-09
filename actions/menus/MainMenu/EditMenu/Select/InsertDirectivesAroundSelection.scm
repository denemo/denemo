;;;; InsertDirectivesAroundSelection
;;;If a selection comprises two standalone directives it puts them on the clipboard and  memorises their types
;;;;;if not it inserts inserts the first and second objects in the clipbaord around the current selection, checking for their types.
      
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
        (d-WarningDialog (string-append (if message message "\n") "\n" (_ "First execute this command with the selection set to two standalone directives (which will then be copied to the clipboard).")
        "\n" (_  "Then select music to be bracketed by these two directives and re-execute the command")))
        (set! message #f))
            
    (d-PushPosition)
    (d-GoToSelectionStart)
    (set! measure-num (d-GetMeasure))
    (if (MoveToEndOfSelection)
        (begin
            (set! within-measure (= measure-num (d-GetMeasure)))
            (if  (Directive?)
                (begin
                    (set! InsertDirectivesAroundSelection::end (d-DirectiveGetForTag-standalone))
                    (d-MoveCursorLeft)
                    (if (Directive?)
                        (begin ;;; we are initializing the system
                            
                            
                            (set! InsertDirectivesAroundSelection::start (d-DirectiveGetForTag-standalone))
                            (d-WarningDialog (string-append (_ "The pair of directives") " " InsertDirectivesAroundSelection::start
                                " " "and" " " InsertDirectivesAroundSelection::end " " "will now be cut to the clipboard. Invoking this command in future will bracket selected music with those two directives."))
                            (d-Cut) ;;;put the two standalones on the clipboard
                            )
                        (begin
                            (set! message (_ "The two objects selected are not standalone directives."))
                            (set! InsertDirectivesAroundSelection::start #f)
                            (set! InsertDirectivesAroundSelection::end #f))))
                ;;;; we have a selection not ending on a directive: surround with the clipboard objects
                (begin
                    (set! selection-end (GetPosition))
                    
                    (if (and InsertDirectivesAroundSelection::start InsertDirectivesAroundSelection::end)
                        (begin 
                            (d-GoToSelectionStart)
                            (if (and (d-PutClipObj 0 0) (d-MoveCursorLeft) (equal? (d-DirectiveGetForTag-standalone) InsertDirectivesAroundSelection::start))
                                (begin
                                    (apply d-GoToPosition selection-end)
                                    (d-MoveCursorRight)
                                    (if within-measure (d-MoveCursorRight));; because we have inserted one clip obj in this measure
                                    (if (and (d-PutClipObj 0 1) (d-MoveCursorLeft) (equal? (d-DirectiveGetForTag-standalone) InsertDirectivesAroundSelection::end))
                                        (set! message #f) ;;; success
                                        (begin
                                            (set! message (_ "Clipboard is messed up, edit by hand"))
                                            (set! InsertDirectivesAroundSelection::start #f)
                                            (set! InsertDirectivesAroundSelection::end #f))))
                                (begin
                                    (d-DeleteObject)
                                    (set! InsertDirectivesAroundSelection::start #f)
                                    (set! InsertDirectivesAroundSelection::end #f)
                                    (set! message (_ "Clipboard has changed and does not hold a pair of directives to bracket the selection. Restart the command.")))))
                        (begin
                            (set! message (_ "No pair of directives available - start again, selecting a pair of directives to be used.")))))))
        (begin
            (set! message (_ "No Selection"))))
    (if message (help))
    (d-PopPosition))