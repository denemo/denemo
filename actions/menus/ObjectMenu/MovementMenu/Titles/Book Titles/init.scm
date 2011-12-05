(define (MovementBookTitles::Do field lilyfield initial help)
	(define tag (string-append "Movement" field))
	(d-LilyPondInclude "book-titling.ily")
	(let ((chapter (d-DirectiveGet-movementcontrol-display tag)))
  (if (not chapter)
    (set! chapter initial))
  (set! chapter (d-GetUserInput initial help chapter))
  (if chapter
   (begin
     (d-SetSaved #f)
     (if (string-null? chapter)
  	(d-DirectiveDelete-movementcontrol tag)
      	(begin 
      		(d-DirectivePut-movementcontrol-display tag  chapter)
      		(d-DirectivePut-movementcontrol-override tag  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
      		(d-DirectivePut-movementcontrol-prefix tag (string-append "\\" lilyfield " \"" chapter "\"\n"))))))))
