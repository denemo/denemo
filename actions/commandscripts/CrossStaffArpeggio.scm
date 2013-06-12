;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;CrossStaffArpeggio
(disp "Entered with " CrossStaffArpeggio::params " ok")
(let ((tag "CrossStaffArpeggio"))
 (if (d-Directive-staff? tag)
 	(begin
 		(d-DirectiveDelete-staff tag)
 		(d-InfoDialog (_ "Arpeggios will not cross staffs")))
 	(begin
 		(d-InfoDialog (_ "Arpeggios will cross staffs"))
 		(d-DirectivePut-staff-display tag (_ " Cross Staff Arpeggios "))
 		(d-DirectivePut-staff-override tag  DENEMO_OVERRIDE_GRAPHIC)
		(d-DirectivePut-staff-postfix tag " \\set Score.connectArpeggios = ##t "))))
(d-SetSaved #f)