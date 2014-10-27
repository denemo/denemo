;;;;;FretDiagram
(let ((tag "FretDiagram")(params FretDiagram::params)(data #f)(number-of-strings #f)(strings '()))
    (define (get-fret string-num fret)
            (define prompt (string-append (_ "String: ") (number->string string-num) " " (_ "Give Fret Number") ))
            (if (not fret)
                (set! fret "x"))
            (set! fret (d-GetUserInput (_ "Fret Diagram") prompt fret))
            (if fret
                fret
                "x"))
    
    (if (equal? params "edit")
        (set! params #f))
    (set! data (d-DirectiveGet-standalone-data tag))
    (if data
        (begin
            (set! data (eval-string data))
            (set! number-of-strings (assq-ref data 'number-of-strings))
            (set! strings (assq-ref data 'strings))))
    (if params
        (begin
            (if (assq-ref params 'number-of-strings)
                  (set! number-of-strings (assq-ref params 'number-of-strings)))
            (if (assq-ref params 'strings)    
                  (set! strings (assq-ref params 'strings)))))
   (if (not number-of-strings)
       (begin
            (set! number-of-strings (d-GetUserInput (_ "Fret Diagram") (_ "Give Number of Strings ") "6"))
            (if (and number-of-strings (string->number number-of-strings))
                (set! number-of-strings (string->number number-of-strings))
                (set! number-of-strings #f))))
   (if (and number-of-strings (< number-of-strings 10))
      (begin       
       (let loop ((count number-of-strings))
            (if (> count 0)
                (begin
                    (set! strings (assq-set! strings count (get-fret count (assq-ref strings count))))
                    (loop (1- count)))))
        (set! data '())
        (set! data (assq-set! data 'number-of-strings number-of-strings))
        (set! data (assq-set! data 'strings strings))
        (let ((markup (format #f "w:~s;" number-of-strings)))
                (let loop ((count number-of-strings))
                    (if (> count 0)
                        (begin
                            (set! markup (string-append markup (number->string count) "-" (assq-ref strings count) ";"))
                            (loop (1- count)))))
                (d-Directive-standalone tag)
                (d-DirectivePut-standalone-postfix tag (format #f "-\\markup \\fret-diagram #~s " markup))
                (d-DirectivePut-standalone-data tag (format #f "'~s" data))
                (d-DirectivePut-standalone-display tag (_ "Frets"))
                (d-DirectivePut-standalone-minpixels tag 50)
                (d-SetSaved #f)
                (d-RefreshDisplay)))))
