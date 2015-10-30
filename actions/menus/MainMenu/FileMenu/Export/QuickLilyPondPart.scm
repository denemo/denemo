(d-PushPosition)
(d-GoToBeginning)
(let ((current "-1")(name  (d-Open "query=filename")))
    (if name
        (let* ((filename  (string-append (substring name 0 (- (string-length name) 7)) "-denemo-" (number->string (d-GetStaff)) ".ly"))   
                (port (open-file filename "w")))
            (let loop ()
		(define measure-number (d-GetMeasure))
                (if port
                    (begin
			(if (not (equal? measure-number current))
				(begin
					(set! current measure-number)
					(format port "%Measure ~A\n" measure-number)))
                        (format port "~A " (d-GetLilyPond))
                        (if (d-MoveCursorRight)
                            (loop)
                            (close-port port))))))
        (d-WarningDialog (_ "The score does not have a file name, so no file name for the output LilyPond file can be constructed. Save the score first."))))
(d-PopPosition) 