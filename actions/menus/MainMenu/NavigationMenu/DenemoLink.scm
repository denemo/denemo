;;;DenemoLink
(let ((tag "DenemoLink")(params DenemoLink::params))
  (if (equal? params "delete")
  	(let ((choice (RadioBoxMenu (cons (_ "Delete") 'delete) (cons (_ "Cancel") 'cancel)))) 
  		(case choice
  			((delete) (d-LockDirective #f) (d-DirectiveDelete-standalone tag))
  			(else (d-WarningDialog (_ "Cancelled")))))
      (if (d-Directive-standalone? tag)
		(let ((link (d-DirectiveGet-standalone-data tag)))
		    (if (not link)
		         (begin
		            (set! link (d-DirectiveGet-standalone-postfix tag))
		            (if link
		                (set! link (string-trim-both link   (lambda (c)(or (eqv? c #\{) (eqv? c #\%))))))))
		    (if link
		        (begin
		            (d-OpenSource link)
		            (d-MoveCursorRight))))
		(begin
		         	(d-PushPosition)
		         	(while (and (not (d-Directive-standalone? tag)) (d-MoveCursorLeft)))
		         	(if (d-Directive-standalone? tag)
		         		(begin
		         			(set! link (d-DirectiveGet-standalone-postfix tag))
		         			 (d-OpenSource link)
		         			 (d-PopPosition))
		         		(begin
		         			(d-PopPosition)
		         			(d-WarningDialog (_ "There is no link here, open the source document and click on it to place one."))))))))
