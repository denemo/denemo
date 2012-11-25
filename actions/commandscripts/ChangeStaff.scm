;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;Change staff-by DRW modified to avoid use of aliases by RTS
(if (d-Directive-standalone? "StaffChange")
	(d-DirectiveDelete-standalone "StaffChange")
	(let ((Choices  "")(Dummy "") (Name #f ))

( define (GetNames) 
	(begin
		(set! Dummy  (d-StaffProperties "query=denemo_name"))
		(if Dummy (set! Choices (string-append Choices Dummy stop)))
		(if (d-MoveToStaffDown) (GetNames))))

(d-PushPosition)
(while (d-MoveToStaffUp))
(GetNames)
(d-PopPosition)
(set! Name (d-GetOption Choices ))
(if Name 
	(begin
		(d-DirectivePut-standalone  "StaffChange")
		(d-DirectivePut-standalone-minpixels  "StaffChange" 50)
		(d-DirectivePut-standalone-postfix "StaffChange" (string-append "\\change Staff=\"" Name "\" " ))
		(d-DirectivePut-standalone-graphic "StaffChange" (string-append 
		"\n⇒" Name "\nDenemo\n12"))
		(d-DirectivePut-standalone-gy  "StaffChange" -1)))
(d-RefreshDisplay)))