;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;PrintAccompanistsScore
(let ((saved (d-GetSaved)) (response (d-GetUserInput "Print Accompanist's Score" "How many staffs to make small?" "1")))
  (if response
      (let ((staffs ""))
	(set! response (string->number response))
	(let loop ((number 1))
	  (set! staffs (string-append staffs "(d-GoToPosition #f " (number->string number) " 1 1) (d-TinyStaff)"))
	  (if (< number response)
	      (loop (+ 1 number))))	
	(ForAllMovements staffs)
	(d-TypesetForScript "(d-PrintAccompanistsScore)")
	(d-CreateLayout (string-append "Accompanist with " (number->string response) " cue part" ))
	(ForAllMovements staffs)
	(d-SetSaved saved))))
