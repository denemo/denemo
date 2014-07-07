;;;TaglineLink
 (let* ((tag "TaglineLink")(link (d-DirectiveGet-scoreheader-data tag))(text #f))
    (if link
        (begin
            (set! link (eval-string link))
            (set! text (car link))
            (set! link (cdr link)))
        (begin
            (set! text (_ "Source file at http://denemo.org"))
            (set! link "http://www.denemo.org")))
            
    (set! link (d-GetUserInput (_ "Tagline with Link") (_ "Give URL to link to") link))
    (set! text (d-GetUserInput (_ "Tagline with Link") (_ "Give text for tagline") text))
    (if link
        (begin
         (if (not text) 
            (set! text "Link"))
         (if (string-null? text)
            (begin
                (d-WarningDialog (_ "Tagline deleted"))
                (d-DirectiveDelete-scoreheader tag))
            (begin
                (d-DirectivePut-scoreheader-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                (d-DirectivePut-scoreheader-display tag (DenemoAbbreviatedString text))
                (d-DirectivePut-scoreheader-data tag (string-append "(cons \"" (scheme-escape text)
                                                                            "\" \"" (scheme-escape link) "\")"))
                (d-DirectivePut-scoreheader-postfix tag (string-append "tagline = \\markup { \\with-url #\"" link "\" \"" text "\"}\n"))))      
        (d-SetSaved #f))))
