;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;MarkupAtEnd
(let ((tag "MarkupAtEnd")(themarkup ""))
        (d-DirectivePut-movementcontrol-override tag (logior
DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
        (d-DirectivePut-movementcontrol-display tag (_ "Markup At End"))
        (disp "checking previous markup ...")
        (if (d-Directive-movementcontrol? tag)
                (set! themarkup (d-DirectiveGet-movementcontrol-postfix tag)))
        (set! themarkup (d-GetUserInputWithSnippets (_ "Markup At End") (_ "Edit markup:") themarkup))
        (if themarkup
        	(begin
        		(set! themarkup (car themarkup))
       			 (disp themarkup)
        		(d-DirectivePut-movementcontrol-postfix tag themarkup)
        		(d-SetSaved #f))
        	(d-InfoDialog "Cancelled")))
