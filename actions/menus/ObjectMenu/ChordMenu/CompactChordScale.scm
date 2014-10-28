;;;CompactChordScale
(let* ((tag "CompactChordScale")(params CompactChordScale::params)(scalex "2.0") (scaley "2.0") (data  (d-DirectiveGet-standalone-data tag)))
    (if data
        (begin
            (d-DirectiveDelete-standalone tag)
            (set! data (eval-string data))
            (set! scalex (car data))
            (set! scaley (cdr data))))
     (if (equal? params "edit")
     	(set! params #f))
    (if params
    	(begin
    		(set! scalex (number->string (car params)))
    		(set! scaley (number->string (cdr params)))
    		(set! params 'finished)))
    (if (not params)	
    	(set! scalex (d-GetUserInput (_ "Chord Symbol Scale") (_ "Give horizontal scaling for next chord") scalex)))
    	
        (if scalex
            (begin
            	(if (not params)
                	(set! scaley (d-GetUserInput (_ "Chord Symbol Scale") (_ "Give vertical scaling for next chord") scaley)))
                (if scaley 
                    (begin
                        (d-DirectivePut-standalone tag)
                        (d-DirectivePut-standalone-postfix tag (string-append "\\once \\set Score.chordCompactScale = #'(" scalex " . " scaley ") "))
                        (d-DirectivePut-standalone-display tag "Scale")
                        (d-DirectivePut-standalone-data tag (format #f "'~s" (cons scalex scaley)))
                        (d-DirectivePut-standalone-minpixels tag 30)
                        (d-SetSaved #f)
                        (d-RefreshDisplay))))))