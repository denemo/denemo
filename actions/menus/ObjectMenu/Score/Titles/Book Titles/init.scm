(define (BookTitles::Do field lilyfield initial help)
	(define tag (string-append "Book" field))
	(d-LilyPondInclude "book-titling.ily")
	(d-LilyPondInclude "simplified-book-titling.ily")
	(let ((chapter (d-DirectiveGet-scoreheader-display tag)))
  (if (not chapter)
    (set! chapter initial))
  (set! chapter (d-GetUserInput initial help chapter))
  (if chapter
   (begin
     (d-SetSaved #f)
     (if (string-null? chapter)
  	(d-DirectiveDelete-scoreheader tag)
      	(begin 
      		(d-DirectivePut-scoreheader-display tag  chapter)
      		(d-DirectivePut-scoreheader-override tag  (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
      		(d-DirectivePut-scoreheader-postfix tag (string-append lilyfield " = \"" chapter "\"\n"))))))))
