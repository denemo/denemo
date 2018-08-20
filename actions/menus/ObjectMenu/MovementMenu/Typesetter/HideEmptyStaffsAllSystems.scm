;;;HideEmptyStaffsAllSystems
(let ((tag "HideEmptyStaffsAllSystems"))
  (if (d-Directive-layout? tag)
      (begin
	(d-DirectiveDelete-layout tag)
	(if (d-Directive-layout? "HideEmptyStaffs")
	    (d-HideEmptyStaffs)))
      (begin
	  (if (not (d-Directive-layout? "HideEmptyStaffs"))
	    (d-HideEmptyStaffs))
	  (d-DirectivePut-layout-postfix tag 
				     " \\context { \\Staff \\override VerticalAxisGroup #'remove-first = ##t } ")
          (d-DirectivePut-layout-override  tag DENEMO_OVERRIDE_GRAPHIC)
	  (d-DirectivePut-layout-display  tag (_ "Empty staffs hidden in first line"))))
  (d-SetSaved #f))
