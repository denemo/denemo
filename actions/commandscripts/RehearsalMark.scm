;;; Bookmark + RehearsalMark
;;; by Nils Gey Modified RTS

(if (d-Directive-standalone? "RehearsalMark")
 (let ((choice #f))
  (begin
    (set! choice (d-GetOption  (string-append (_ "Delete") stop (_ "Restore Position") stop  cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (d-DirectiveTextEdit-standalone "RehearsalMark"))
     ((equal? choice (_ "Delete"))
      (d-DirectiveDelete-standalone "RehearsalMark"))
     ((equal? choice (_ "Restore Position"))
      (d-DirectivePut-standalone-prefix "RehearsalMark" "")))
    (if choice
			(begin (d-SetSaved #f)
				(d-RefreshDisplay)))))
;;not present already
 (begin
		(d-Directive-standalone "RehearsalMark")
		(d-DirectivePut-standalone-display "RehearsalMark" "")
		(d-DirectivePut-standalone-postfix "RehearsalMark"  " \\mark \\default" )
		(d-DirectivePut-standalone-gx  "RehearsalMark"  14)
		(d-DirectivePut-standalone-gy  "RehearsalMark"  -35)
		(d-DirectivePut-standalone-minpixels  "RehearsalMark"  30)
		(d-DirectivePut-standalone-graphic "RehearsalMark" "RehearsalMark")
		(d-MoveCursorRight) 
		(d-SetSaved #f)
		(d-RefreshDisplay)))
