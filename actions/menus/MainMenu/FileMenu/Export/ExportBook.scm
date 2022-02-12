;;;;;CreateBook
(define-once CreateBook::page-start 1)
(define-once CreateBook::pdfs "")

(let ( (tag "CreateBook") (choice 
	(RadioBoxMenu (cons (_ "Add Current Layout to Book") 'add)
					(cons (_ "Create PDF from Book") 'create)
					(cons (_ "Start a new Book") 'new))))
	(define (create-pdf)
			(let* ((basename (tmpnam))
							(lilyfile (string-append basename ".ly")))
						(set! CreateBook::pdfs (string-append CreateBook::pdfs " " basename ".pdf"))
						(d-ExportMUDELA lilyfile)
						(d-WarningDialog (_ "Click \"Close\" and this dialog will freeze until the typesetting is done\nSorry this is so clunky!"))
						(d-CreatePDFFromLilyfile lilyfile basename)
						(set! CreateBook::page-start (+ 0 CreateBook::page-start (d-GetNumberTypesetPages)))
						(d-InfoDialog (string-append (_"You have a book with ") (number->string (1- CreateBook::page-start)) (_ " page(s).
If you want to add another layout to the book typeset it and run this command again.
Or run the command again to export the book as a PDF.")))))						
	(case choice
		((add)  
				(if (= 1 CreateBook::page-start)
					(create-pdf)
					(begin
						(d-DirectivePut-paper-postfix tag (string-append "\nprint-first-page-number=##t\nfirst-page-number = " (number->string CreateBook::page-start) "\n"))
						(create-pdf)
						(d-DirectiveDelete-paper tag))))		
		((new) 
				(d-InfoDialog (_ "You have a new (empty) book. Choose a layout, typeset it (if you haven't already), and run this command again to place it as the first in the book."))
				(set! CreateBook::page-start 1)
				(set! CreateBook::pdfs ""))
		((create)
				(if (string-null? CreateBook::pdfs)
					(d-WarningDialog (_ "You must add layouts to the Book first"))
					(let* ((name (d-GetUserInput (_ "Create Book") (_ "Give name for PDF:") "DenemoBook")) 
						(output (d-ChooseDirectory (string-append (_ "Choose where to save ") name ".pdf")))
						(command (if output (string-append "gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=\"" output "/" name ".pdf" "\"" CreateBook::pdfs) #f)))
						(if command
							(system command)
							(d-WarningDialog (_ "Score saved to disk"))))))))
