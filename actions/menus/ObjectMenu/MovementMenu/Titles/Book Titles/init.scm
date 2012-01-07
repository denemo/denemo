(define (MovementBookTitles::Do field lilyfield initial help)
	(d-LilyPondInclude "book-titling.ily")
	(d-LilyPondInclude "simplified-book-titling.ily")
	(let ((chapter (d-DirectiveGet-movementcontrol-display field)))
  (if (not chapter)
    (set! chapter initial))
  (set! chapter (d-GetUserInput initial help chapter))
  (if chapter
   (begin
     (d-SetSaved #f)
     (if (string-null? chapter)
  	(d-DirectiveDelete-movementcontrol field)
      	(begin 
      		(d-DirectivePut-movementcontrol-display field  chapter)
      		(d-DirectivePut-movementcontrol-override field  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
      		(d-DirectivePut-movementcontrol-prefix field (string-append "\\" lilyfield " \"" chapter "\"\n"))))))))
