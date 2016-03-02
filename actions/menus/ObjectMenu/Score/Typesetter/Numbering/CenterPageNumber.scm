;;;CenterPageNumber
(let ((tag "CenterPageNumber"))
	(if (d-Directive-paper? tag)
		(begin
			(d-DirectiveDelete-paper tag)
			(d-InfoDialog (_ "Default page number position reinstated")))
             (d-DirectivePut-paper-postfix tag "
    print-page-number = ##t
    print-first-page-number = ##t
    oddHeaderMarkup = \\markup \\null
    evenHeaderMarkup = \\markup \\null
    oddFooterMarkup = \\markup {
      \\fill-line {
        \\on-the-fly \\print-page-number-check-first
        \\fromproperty #'page:page-number-string
      }
    }
    evenFooterMarkup = \\oddFooterMarkup
          "))
(d-SetSaved #f))
