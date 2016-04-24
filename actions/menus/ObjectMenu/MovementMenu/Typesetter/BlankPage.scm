;;; BlankPage
(let* ((tag "BlankPage") 
    (params  BlankPage::params)(text #f)(data (d-DirectiveGet-movementcontrol-data tag)))
    
  (if (equal? params "edit")
    (set! params #f))
  (if (not params)
    (set! params data))
  (if (and (not data) (not params))
    (begin
        (set! params  (d-GetUserInput  (_ "Blank Page") (_ "Give text to appear on blank page, if any ") (_ "This page is intentionally left blank")))
        (if params
            (set! params (string-append "\\vspace #20 \\bold \\fontsize #8 {" params " }")))))
  (if (not params)
     (set! params ""))
  (if data
    (let ((choice (RadioBoxMenu (cons (_ "Edit Text") 'edit) (cons (_ "Delete") 'delete))))
        (case choice
            ((edit)
                 (set! params (d-GetUserInputWithSnippets (_ "Blank Page") (_ "Give text to appear on blank page, if any ") data))
                 (if params
                    (set! params (car params))))
            ((delete)
                (set! params #f)
                (d-DirectiveDelete-movementcontrol tag))
            (else
                (d-InfoDialog (_ "Cancelled"))))))
   (if params
        (begin
             ;;; (d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
              (d-DirectivePut-movementcontrol-data tag params)
              (d-DirectivePut-movementcontrol-display tag params)
              (d-DirectivePut-movementcontrol-prefix  tag (string-append "\n\\pageBreak \\markup \\with-url #'\"scheme:(d-BlankPage)\"{" params "} \\pageBreak\n"))))
   (d-SetSaved #f))
