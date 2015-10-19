;;;SystemSystemPadding
(let ((tag "SystemSystemPadding") (value #f))
(define (SystemSystemPadding::set value)
	(d-DirectivePut-paper-display tag value)
	(d-DirectivePut-paper-postfix tag (string-append "
system-system-spacing #'padding = #" value "\n")))
(if (or (not SystemSystemPadding::params) (equal? SystemSystemPadding::params "edit"))
	(begin
		(set! SystemSystemPadding::params #f)
		(set! value (d-DirectiveGet-paper-display tag))))
(if SystemSystemPadding::params
	(begin
		(SystemSystemPadding::set (number->string  SystemSystemPadding::params)))
  (begin
    (if (not value)
      (set! value "5"))
		(set! value (d-GetUserInput (_ "Padding Between Systems") (_ "Give extra padding (1=no extra space, staff units)") value))))
(disp "value is " value "\n")
(if (and (string? value) (string->number value))
		 (SystemSystemPadding::set  value)))	
(d-SetSaved #f)
