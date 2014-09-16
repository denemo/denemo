;;; Bookmark + RehearsalMark
;;; by Nils Gey Modified RTS
(let ((tag "RehearsalMark"))
(if (d-Directive-standalone? tag)
 (let ((choice #f))
  (begin
    (set! choice (d-GetOption  (string-append cue-NudgePosition stop cue-RestorePosition stop cue-Delete stop cue-Advanced stop)))
    (cond
     ((equal? choice  cue-Advanced)
      (if (not (d-DirectiveTextEdit-standalone tag))
        (d-DirectiveDelete-standalone tag)))
     ((equal? choice cue-Delete)
      (d-DirectiveDelete-standalone tag))
     ((equal? choice cue-NudgePosition)
        (let ((amount (GetNudge)))
            (if amount
                (TweakRelativeOffset tag (car amount) (cdr amount))
                (d-WarningDialog (_ "Operation cancelled")))))
      
     ((equal? choice cue-RestorePosition)
         (d-DirectivePut-standalone-prefix tag "")
         (d-DirectivePut-standalone-data tag ""))
      (else 
        (d-WarningDialog (_ "Operation cancelled"))))
    (if choice
            (begin (d-SetSaved #f)
                (d-RefreshDisplay)))))
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
