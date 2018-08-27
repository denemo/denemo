;;;HideEmptyStaffsAllSystems
(let ((tag "HideEmptyStaffsAllSystems"))
  (if (d-Directive-layout? tag)
      (let ((Hes (not (eq? (d-DirectiveGet-layout-data tag) "'HideEmptyStaffs"))))
	(d-DirectiveDelete-layout tag)
	(if (and (d-Directive-layout? "HideEmptyStaffs") Hes)
	    (d-HideEmptyStaffs)))
      (begin
	  (if (not (d-Directive-layout? "HideEmptyStaffs"))
	    (d-HideEmptyStaffs)
	    (d-DirectivePut-layout-data tag "'HideEmptyStaffs"))
	  (d-DirectivePut-layout-postfix tag 
				     " \\context { \\Staff \\override VerticalAxisGroup #'remove-first = ##t } ")
          (d-DirectivePut-layout-override  tag DENEMO_OVERRIDE_GRAPHIC)
	  (d-DirectivePut-layout-display  tag (_ "Empty staffs hidden in all systems"))))
  (d-SetSaved #f))
