;;DeleteCurrentStaff
(let ((delete (RadioBoxMenu (cons (_"Cancel") #f) (cons (_ "Delete Current Staff") 'delete))))
	(if delete
		(d-DeleteStaff)
		(d-InfoDialog (_ "Cancelled"))))
