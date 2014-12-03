  ;;;;;;;;;;;;;;;Toggle HideEmptyStaffs
(let ((tag "HideEmptyStaffs"))
  (if (d-Directive-layout? tag)
      (d-DirectiveDelete-layout tag)
      (begin
	  (d-DirectivePut-layout-postfix tag 
				     " \\context { \\Staff \\RemoveEmptyStaves } ")
          (d-DirectivePut-layout-override  tag DENEMO_OVERRIDE_GRAPHIC)
	  (d-DirectivePut-layout-display  tag (_ "Empty Systems Hidden"))))
  (d-SetSaved #f))
