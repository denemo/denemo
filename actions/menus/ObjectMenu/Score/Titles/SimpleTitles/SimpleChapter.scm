;;;SimpleChapter
(let* ((tag "SimpleChapter")(thetitle (d-DirectiveGet-movementcontrol-data tag)))
  (define (get-title title)
    (let ((input (d-GetUserInputWithSnippets (_ "Chapter")(_ "Give chapter title::") title)))
         (if input
                      (begin
                        (set! input (car input))
                        (if (zero? (string-length input)) ;; if empty title string => cancel
                            (set! input #f))))
    input))
  (define (put-chapter-start title)
        (set! title (get-title title))
        (if title
            (begin
                (d-DirectivePut-movementcontrol-data tag  title)
                (d-DirectivePut-movementcontrol-display tag  title)
                (d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
                (d-DirectivePut-movementcontrol-prefix tag (string-append "\\bookpart { %start of chapter\n  \\header {\n    title = \\markup { "
                                                title " }\n  }\n"))
                (d-SetSaved #f))
            (begin
                    (set! title 'finished)
                    (d-InfoDialog "Cancelled")))
        title)
  (define (put-chapter-end)
    (let ((end (d-GetUserInput (_ "Chapter") "Give Chapter End Movement Number: " (number->string (1+ (d-GetMovement))))))
        (d-PushPosition)
        (if (and (d-NextMovement) end (string->number end) (>= (string->number end) (d-GetMovement)))
            (begin
                (set! end (string->number end))
               
                (while (and (> end (d-GetMovement)) (d-NextMovement)))
                (if (> end (d-GetMovement))
                	(begin
                		(d-WarningDialog (_ "Setting End Chapter at last movement"))
                		(set! end (d-GetMovement))))
                (if (d-Directive-movementcontrol? tag)
                    (begin
                        (d-WarningDialog (string-append (_ "A Chapter start/end is already present at movement #") (number->string (d-GetMovement))))
                        (set! end #f)))
                (if (and end (= end (d-GetMovement)))
                    (begin 
                        (d-DirectivePut-movementcontrol-display tag  (_ "End Chapter"))   
                        (d-DirectivePut-movementcontrol-override tag (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_TAGEDIT))
                        (d-DirectivePut-movementcontrol-postfix tag "\n} %end of chapter\n"))))
            (set! end #f))
         (d-PopPosition)
         end))
    
    (define (delete-chapter-end)
         (d-PushPosition)
         (while (and (d-NextMovement) (not (d-Directive-movementcontrol? tag)))) ;;look for a chapter directive
         (if (d-DirectiveGet-movementcontrol-postfix tag) ;;; it is an end
                (d-DirectiveDelete-movementcontrol tag)
            (d-WarningDialog (_ "Miss-matched Chapter Start/End, use Movement Properties Editor to fix this")))
         (d-PopPosition))

    (define (delete-chapter)
              (if (or (not (d-Directive-movementcontrol? tag)) (d-DirectiveGet-movementcontrol-postfix tag)) 
                (d-WarningDialog (_ "Miss-matched Chapter Start/End, use Movement Properties Editor to fix this"))
                    (begin
                        (d-SetSaved #f)
                        (d-DirectiveDelete-movementcontrol tag)
                        (delete-chapter-end))))
;;;;;;; procedure starts here                                              
  (if thetitle ;;;already at a chapter start
    (let ((choice (RadioBoxMenu (cons (_ "Re-title") 'retitle) (cons (_ "Delete") 'delete))))
        (case choice 
            ((delete)
                (set! thetitle 'finished)
                (delete-chapter))
            ((retitle)
                (d-SetSaved #f)
                (set! thetitle (put-chapter-start thetitle))
                (set! thetitle 'finished))
            (else
                    (set! thetitle 'finished)
                    (d-InfoDialog "Cancelled")))))
    (if (not (eq? thetitle 'finished))
        (begin
            (if (d-DirectiveGet-movementcontrol-postfix tag)
                (d-WarningDialog (_ "This is the end of a Chapter, start the next chapter on the next movement"))
                (let ((end (put-chapter-end)))
                    (if end
                        (begin
                            (set! thetitle (put-chapter-start (_ "Chapter Title")))
                            (if (eq? thetitle 'finished)
                                (delete-chapter-end)))))))))
               
 
