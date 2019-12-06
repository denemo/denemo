;;;DenemoLink
(let ((tag "DenemoLink")(params DenemoLink::params))
  (if (equal? params "delete")
  	(let ((choice (RadioBoxMenu (cons (_ "Delete") 'delete) (cons (_ "Cancel") 'cancel)))) 
  		(case choice
  			((delete) (d-LockDirective #f) (d-DirectiveDelete-standalone tag))
  			(else (d-WarningDialog (_ "Cancelled")))))
      (if (d-Directive-standalone? tag)
		(begin
		    (DenemoFollowLink)
		    (d-MoveCursorRight))
		(let ((num (d-GetMeasure)))
		         	(d-PushPosition)
		         	(while (and (not (d-Directive-standalone? tag)) (d-MoveCursorLeft)))
		         	(if (d-Directive-standalone? tag)
		         		(begin
		         			(DenemoFollowLink)
						(if (> num (d-GetMeasure))
						  (d-InfoDialog (string-append (_ "The cursor is ") (number->string (- num (d-GetMeasure))) " bars after the location marked in the source.")))
						(d-PopPosition))
		         		(begin
		         			(d-PopPosition)
		         			(d-WarningDialog (_ "There is no link before this bar in this staff, open the source document and click on it to place one."))))))))
