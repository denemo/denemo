;;;;;ExportBook
(define-once ExportBook::page-start 1)
(define-once ExportBook::pdfs "")
(define-once ExportBook::layouts "")

(let ( (saved (d-GetSaved))(tag "ExportBook") (choice 
	(RadioBoxMenu (cons (_ "Add Current Layout to Book") 'add)
					(cons (_ "Create PDF from Book") 'create)
					(cons (_ "Start a new Book") 'new))))
	(define (create-pdf)
			(let* ((basename (tmpnam))
							(lilyfile (string-append basename ".ly")))
						(set! ExportBook::pdfs (string-append ExportBook::pdfs " " basename ".pdf"))
						(set! ExportBook::layouts (string-append ExportBook::layouts ", \"" (d-GetLayoutName) "\""))
						(d-ExportMUDELA lilyfile)
						
						(d-WarningDialog (_ "Hit Enter or Click \"Close\" and this dialog will freeze until the typesetting is done\nSorry this is so clunky."))
						
						(d-CreatePDFFromLilyfile lilyfile basename)
						
						(set! ExportBook::page-start (+ 0 ExportBook::page-start (d-GetNumberTypesetPages)))
						
						(d-InfoDialog (string-append (_"You have a book with ") (number->string (1- ExportBook::page-start)) (_ " page(s); it includes the layout(s)") ExportBook::layouts (_ " so far.
If you want to add another layout to the book typeset it and run this command again.
Or run the command again to export the book as a PDF.")))))

						
	(case choice
		((add)  
				(if (= 1 ExportBook::page-start)
					(create-pdf)
					(begin
						(d-DirectivePut-paper-postfix tag (string-append "\nprint-first-page-number=##t\nfirst-page-number = " (number->string ExportBook::page-start) "\n"))
						(create-pdf)
						
						(d-DirectiveDelete-paper tag))))		
		((new) 
				(d-InfoDialog (_ "You have a new (empty) book. Choose a layout, typeset it (if you haven't already), and run this command again to place it as the first in the book."))
				(set! ExportBook::page-start 1)
				(set! ExportBook::layouts "")
				(set! ExportBook::pdfs ""))
		((create)
				(if (string-null? ExportBook::pdfs)
					(d-WarningDialog (_ "You must add layouts to the Book first"))
					(let* ((gs (if (equal? DENEMO_OS "Windows") (string-append DENEMO_BIN_DIR "/gs.exe") "gs"))(name (d-GetUserInput (_ "Create Book") (_ "Give name for PDF:") "DenemoBook")) 
						(output (d-ChooseDirectory (string-append (_ "Choose where to save ") name ".pdf")))
						(command (if output (string-append gs " -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=\"" output "/" name ".pdf" "\"" ExportBook::pdfs) #f)))
						(if command
							(system command)
							(d-WarningDialog (_ "Score saved to disk")))))))
		(d-SetSaved saved))
