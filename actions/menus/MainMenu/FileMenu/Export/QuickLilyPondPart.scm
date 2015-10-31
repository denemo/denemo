;;;QuickLilyPondPart
(d-PushPosition)
(d-MoveToBeginning)
(let ((current "-1")(name  (d-Open "query=filename")))
    (if name
        (let* ((filename  (string-append (substring name 0 (- (string-length name) 7)) "-denemo-"
        			 (number->string (d-GetMovement)) "-"
        			  (number->string (d-GetStaff)) ".ly"))   
                (port (open-file filename "w")))
            (if port
                   (begin
		           (format port "%Movement ~A Staff ~A" (d-GetMovement) (d-GetStaff))
			    (let loop ()
				(define measure-number (d-GetMeasure))
				    (begin
					    (if (not (equal? measure-number current))
						(begin 
						    (set! current measure-number)
						    (format port "\n")
						    (if (zero? (modulo current 10))
							(format port "%Measure ~A\n" measure-number))))
					(let ((lily (d-GetLilyPond)))
						(if lily
							(format port "~A " (d-GetLilyPond))
							(format port "%Empty Measure\n")))
							
					(if (d-NextObject)
						(loop)
						(close-port port)))))
        		(d-WarningDialog (_ "The score does not have a file name, so no file name for the output LilyPond file can be constructed. Save the score first."))))))
(d-PopPosition) 
