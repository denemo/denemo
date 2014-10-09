;;;FilledTitleLine
(let ((tag "FilledTitleLine")(params FilledTitleLine::params)(first #f)(second #f)(third #f))
    (define (get-element prompt current)
        (let ((bold #f)(italic #f)(fontsize "1")(value (d-GetUserInput (_ "Filled Title Line") prompt current)))
            (if value
                (begin
                    (set! bold (RadioBoxMenu (cons (_ "Bold") "\\bold ") (cons (_ "Normal") "")))
                    (set! italic (RadioBoxMenu (cons (_ "Italic") "\\italic ") (cons (_ "Upright") "")))
                    (set! fontsize (d-GetUserInput (_ "Font Magnification") (_ "Give font size required") fontsize))
                    (if (and bold italic (string? fontsize) (string->number fontsize))
                        (begin
                            (string-append bold italic "\\fontsize #" fontsize " {" value "} "))
                        #f))
                #f)))
    (set! first (get-element (_ "Give text to appear at the left") "v.1"))
    (if first
        (begin
                (set! second (get-element (_ "Give text to appear in the center") "MyTitle"))
                (if second
                    (begin
                        (set! third (get-element (_ "Give text to appear at the right") "MyComposer"))
                        (if third
                            (begin
                                (d-SetSaved #f)
                                (d-DirectivePut-score-override tag  DENEMO_OVERRIDE_AFFIX)
                                (d-DirectivePut-score-prefix tag  (string-append "\\markup \\fill-line {\\line{" first "}\\line{" second "}\\line{" third "}")))))))))

    