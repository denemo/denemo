;;;SystemsPerPage
(let ((tag "SystemsPerPage")(params  SystemsPerPage::params)(value #f))
	(if (and params (not (equal? params "edit")))
		(set! value params)
		(set! value (d-GetUserInput (_ "Systems Per Page") (_ "Give required systems (lines) per page: ") "10")))
	(if (and value (string->number value))
		(begin
			(d-DirectivePut-paper-display tag (_ "Systems Per Page"))
			(d-DirectivePut-paper-postfix tag	(string-append "
 systems-per-page = " value "\n"))
			(d-SetSaved #f))))
