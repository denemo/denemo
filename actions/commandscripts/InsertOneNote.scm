;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;InsertOneNote
(if (and (Appending?) (d-GetNonprinting))
	(eval-string (string-append "(d-" (string-upcase (d-GetCursorNote)) ")"))
	(eval-string (string-append "(d-Insert" (number->string (d-GetPrevailingDuration)) ")")))