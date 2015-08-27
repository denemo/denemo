;;; Bookmark + RehearsalMark
;;; by Nils Gey Modified RTS
(let ((tag "RehearsalMark"))
(if (d-Directive-standalone? tag)
 (let ((choice (RadioBoxMenu 
            (cons (_ "Nudge Position") 'nudge)
            (cons   (_ "Restore Position") 'restore)
            (cons   (_ "Delete") 'delete)
            (cons   (_ "More Editing Options") 'more)
            (cons   (_ "Advanced") 'advanced))))
    (case choice
        ((more)
            (set! choice #f)
            (d-EditSimilar))
        ((advanced)
          (set! choice #f)
          (if (not (d-DirectiveTextEdit-standalone tag))
            (d-DirectiveDelete-standalone tag)))
        ((delete)
          (d-DirectiveDelete-standalone tag))
        ((nudge)
            (let ((amount (GetNudge)))
                (if amount
                    (TweakOffset tag tag (car amount) (cdr amount));;(TweakRelativeOffset tag (car amount) (cdr amount))
                    (d-WarningDialog (_ "Operation cancelled")))))
        ((restore)
             (d-DirectivePut-standalone-prefix tag "")
             (d-DirectivePut-standalone-data tag ""))
        (else
            (set! choice #f)
            (d-WarningDialog (_ "Operation cancelled"))))
    (if choice
            (begin (d-SetSaved #f)
                (d-RefreshDisplay))))
;;not present already
 (begin
        (d-Directive-standalone tag)
        ;(d-DirectivePut-standalone-display tag "")
        (d-DirectivePut-standalone-grob tag "RehearsalMark")
        (d-DirectivePut-standalone-postfix tag  " \\mark \\default" )
        (d-DirectivePut-standalone-gx  tag  14)
        (d-DirectivePut-standalone-gy  tag  -35)
        (d-DirectivePut-standalone-minpixels  tag  30)
        (d-DirectivePut-standalone-graphic tag "RehearsalMark")
        (d-MoveCursorRight) 
        (d-SetSaved #f)
        (d-RefreshDisplay))))
