;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;HideKeysig
;(let ((lilycontext (d-GetOption  (string-append "Score" stop "Staff" stop "Voice" stop))))
(let ((lilycontext "Staff"))
(if lilycontext
	(begin 
		(if (d-Directive-keysig? "HideKeysig")
 			(d-DirectiveDelete-keysig "HideKeysig")
 			(begin
 				(d-DirectivePut-keysig-prefix "HideKeysig"  (string-append  "\\once \\override " lilycontext ".KeySignature #'stencil = ##f"  ))
 				(d-DirectivePut-keysig-gy "HideKeysig" -10)
 				(d-DirectivePut-keysig-graphic "HideKeysig" "\n⋂\nDenemo\n24")))
 			(d-SetSaved #f))))
 			