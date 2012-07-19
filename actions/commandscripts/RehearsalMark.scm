;;; Bookmark + RehearsalMark
;;; by Nils Gey Modified RTS

(if (d-Directive-standalone? "RehearsalMark")
(let ((choice #f))
  (begin
    (set! choice (d-GetOption  (string-append "Delete" stop "Offset the Position" stop "Set Padding" stop cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-standalone "RehearsalMark"))
     ((equal? choice "Delete")
      (d-DirectiveDelete-standalone "RehearsalMark"))
     ((equal? choice "Offset the Position")
      (ExtraOffset "RehearsalMark" "standalone" "Score."))
     ((equal? choice "Set Padding")
      (SetPadding "RehearsalMark" "standalone" "Score.")))
    (if (not (boolean? choice))
	(d-RefreshDisplay))))

(begin
(d-Directive-standalone "RehearsalMark")
(d-DirectivePut-standalone-display "RehearsalMark" "")
(d-DirectivePut-standalone-postfix "RehearsalMark"  " \\mark \\default" )
(d-DirectivePut-standalone-gx  "RehearsalMark"  14)
(d-DirectivePut-standalone-gy  "RehearsalMark"  -35)
(d-DirectivePut-standalone-minpixels  "RehearsalMark"  30)
(d-DirectivePut-standalone-graphic "RehearsalMark" "RehearsalMark")
(d-MoveCursorRight)
(d-RefreshDisplay)))
