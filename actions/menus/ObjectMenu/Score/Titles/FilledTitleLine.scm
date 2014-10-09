;;;FilledTitleLine
(let* ((tag "FilledTitleLine")(params FilledTitleLine::params)(first #f)(second #f)(third #f)(data (d-DirectiveGet-score-data tag)))
    (define (get-element prompt current)
        (let ((bold #f)(italic #f)(fontsize "0")(value (d-GetUserInput (_ "Filled Title Line") prompt current)))
            (if value
                (begin
                    (set! bold (RadioBoxMenu (cons (_ "Bold") "\\bold ") (cons (_ "Normal") "")))
                    (set! italic (RadioBoxMenu (cons (_ "Italic") "\\italic ") (cons (_ "Upright") "")))
                    (set! fontsize (d-GetUserInput (_ "Font Magnification") (_ "Give font magnification required (+/-)") fontsize))
                    (if (and bold italic (string? fontsize) (string->number fontsize))
                        (begin
                            (cons value (string-append bold italic "\\fontsize #" fontsize " {" value "} ")))
                        #f))
                #f)))
    (if data
        (begin
            (set! data (eval-string data))
            (set! first (assq-ref data 'first))
            (set! second (assq-ref data 'second))
            (set! third (assq-ref data 'third)))
        (begin
            (set! first "v.1")
            (set! second "MyTitle")
            (set! third "MyComposer")))
    (set! first (get-element (_ "Give text to appear at the left") first))
    (if first
        (begin
                (set! second (get-element (_ "Give text to appear in the center") second))
                (if second
                    (begin
                        (set! third (get-element (_ "Give text to appear at the right") third))
                        (if third
                            (let ((data '()))
                                (set! data (assq-set! data 'first (car first)))
                                (set! data (assq-set! data 'second (car second)))
                                (set! data (assq-set! data 'third (car third)))
                                (d-SetSaved #f)
                                (d-DirectivePut-score-data tag (format #f "'~s" data))
                                (d-DirectivePut-score-override tag  DENEMO_OVERRIDE_AFFIX)
                                (d-DirectivePut-score-prefix tag  (string-append "\\markup \\fill-line {\\line{" (cdr first) "}\\line{" (cdr second) "}\\line{" (cdr third) "}}")))))))))