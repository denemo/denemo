;;;MinimumSystemsPerPage
(let ((tag "MinimumSystemsPerPage")(num (d-GetUserInput "Systems Per Page" "Give minimum number:" "2")))
  (if (and num (string->number num) (> (string->number num) 0))
		(begin
		  (d-DirectivePut-paper-postfix tag (string-append "\nmin-systems-per-page  = " num "\n"))
		  (d-DirectivePut-paper-override tag DENEMO_OVERRIDE_GRAPHIC)
		)
		(d-DirectiveDelete-paper tag))
  (d-SetSaved #f))