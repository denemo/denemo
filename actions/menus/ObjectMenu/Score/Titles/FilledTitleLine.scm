;;;FilledTitleLine
(let* ((tag "FilledTitleLine")(params FilledTitleLine::params)(first #f)(second #f)(third #f)(data (d-DirectiveGet-score-data tag)))
    (define (get-element prompt current)
        (define spacer (cdr current))
        (set! current (car current))
        (if spacer
            (begin
                (set! spacer (d-GetUserInput (_ "Space Above") (_ "Extra space above this line: ") spacer))
                 (if (or (not spacer) (not (string->number spacer)))
                                (set! spacer "0"))))
        (let ((value (d-GetUserInputWithSnippets (_ "Text") prompt current)))
            (if value
                (let ((stripped #f))
                    (set! value (car value))
                    (set! stripped (string-copy value))

                    (if spacer
                        (set! stripped (string-append "\\vspace #" spacer " " stripped)))
                    (list value spacer stripped))
                #f)))
    (if data
        (begin
            (set! data (eval-string data))
            (set! first (assq-ref data 'first))
            (set! second (assq-ref data 'second))
            (set! third (assq-ref data 'third)))
        (begin
            (set! first (cons "v.1" "0"))
            (set! second (cons "MyTitle" #f))
            (set! third (cons "MyComposer" #f))))
     (if (string? first) ;backward compatibility
                (begin
                    (set! first (cons first "0"))
                    (set! second (cons second #f))
                    (set! third (cons third #f))))
    (set! first (get-element (_ "Give text to appear at the left\nLilyPond syntax can be included.") first))
    (if first
        (begin
                (set! second (get-element (_ "Give text to appear in the center\nLilyPond syntax can be included.") second))
                (if second
                    (begin
                        (set! third (get-element (_ "Give text to appear at the right\nLilyPond syntax can be included.") third))
                        (if third
                            (let ((data '()))
                                (set! data (assq-set! data 'first (cons (car first) (cadr first))))
                                (set! data (assq-set! data 'second (cons (car second) (cadr second))))
                                (set! data (assq-set! data 'third (cons (car third) (cadr third))))
                                (d-SetSaved #f)
                                (d-DirectivePut-score-data tag (format #f "'~s" data))
                                (d-DirectivePut-score-override tag  DENEMO_OVERRIDE_AFFIX)
                                (d-DirectivePut-score-prefix tag  (string-append "\\markup \\fill-line {\\line{" (caddr first) "}\\line{" (caddr second) "}\\line{" (caddr third) "}}")))))))))
