;;;TaglineLink
 (let ((tag "TaglineLink"))
 (define link (d-GetUserInput "Tagline with Link" "Give URL to link to" "http://www.denemo.org"))
 (define text (d-GetUserInput "Tagline with Link" "Give text for tagline" "Source file at http://denemo.org"))
 (if link
 	(begin
		 (if (not text) 
 		(set! text "Link"))
 		(if (string-null? text)
 			(begin
 				(d-DirectiveDelete-scoreheader tag))
 			(begin
				 (d-DirectivePut-scoreheader-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
  				(d-DirectivePut-scoreheader-display tag text)
  				(d-DirectivePut-scoreheader-postfix tag (string-append "tagline = \\markup { \\with-url #\"" link "\" \"" text "\"}\n"))))		
		(d-SetSaved #f))))
