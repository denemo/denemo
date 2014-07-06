(define (MovementBookTitles::Do field lilyfield initial help)
    (define (get-current tag)
    	(let ((value (d-DirectiveGet-movementcontrol-data tag)))
    		(if value
    			value
    			(d-DirectiveGet-movementcontrol-display tag))))
    (d-LilyPondInclude "simplified-book-titling.ily")
    (let ((chapter (get-current field)))
  (if (not chapter)
    (set! chapter initial))
  (set! chapter (d-GetUserInput initial help chapter))
  (if chapter
   (begin
     (d-SetSaved #f)
     (if (string-null? chapter)
                (d-DirectiveDelete-movementcontrol field)
        (begin 
            (d-DirectivePut-movementcontrol-data field  chapter)
            (d-DirectivePut-movementcontrol-display field  (DenemoAbbreviatedString chapter))
            (d-DirectivePut-movementcontrol-override field  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                    (d-DirectivePut-movementcontrol-prefix field (string-append "\\" lilyfield " \\markup { \\with-url #'\"scheme:(d-" field   ")\" "  "\"" chapter "\"}\n"))))))))

