;;; Bookmark + RehearsalMark
;;; by Nils Gey Modified RTS
(let ((tag "RehearsalMark"))
(if (d-Directive-standalone? tag)
 (let ((choice #f))
  (begin
    (set! choice (d-GetOption  (string-append cue-Delete stop cue-RestorePosition stop  cue-Advanced stop)))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))
     ((equal? choice  cue-Advanced)
      (if (not (d-DirectiveTextEdit-standalone tag))
        (d-DirectiveDelete-standalone tag)))
     ((equal? choice cue-Delete)
      (d-DirectiveDelete-standalone tag))
     ((equal? choice cue-RestorePosition)
      (d-DirectivePut-standalone-prefix tag "")))
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
