;;;;;FretDiagram
(let ((tag "FretDiagram")
        (params FretDiagram::params)
        (data #f)
        (number-of-strings #f)
        (scale 1)        
        (x-offset #f)
        (y-offset #f)
        (prefix "<>")
        (capo #f)
        (strings '()))
    (define (get-fret string-num fret)
            (define prompt (string-append (_ "String: ") (number->string string-num) " " (_ "Give Fret Number") ))
            (if (not fret)
                (set! fret "x"))
            (set! fret (d-GetUserInput (_ "Fret Diagram") prompt fret))
            (if fret
                fret
                "x"))
    (define (do-capo)
        (define lowest (if capo (list-ref capo 0) 1))
        (define highest (if capo (list-ref capo 1) number-of-strings))
        (define fret (if capo (list-ref capo 2) 1))
        (set! lowest (d-GetUserInput (_ "Fret Diagram") (_ "Barre from string (or cancel for none) ") (number->string lowest)))
        (if lowest
                (begin
                    (set! highest (d-GetUserInput (_ "Fret Diagram") (_ "Barre to string (or cancel for none) ") (number->string highest)))
                    (if highest
                        (begin
                            (set! fret (d-GetUserInput (_ "Fret Diagram") (_ "Barre at fret (or cancel for none) ") (number->string fret)))
                            (set! capo (list (string->number lowest) (string->number highest) (string->number fret))))
                        (set! capo #f)))
                (set! capo #f)))
    (define (do-scale)
            (set! scale (d-GetUserInput (_ "Fret Diagram") (_ "Give size for diagram ") (number->string scale)))
            (if (and scale (string->number scale))
                (set! scale (string->number scale))
                (set! scale 1)))                
    (if (equal? params "edit")
        (set! params #f))
    (set! data (d-DirectiveGet-standalone-data tag))
    (if data
        (begin
            (set! data (eval-string data))
            (set! number-of-strings (assq-ref data 'number-of-strings))
            (set! capo (assq-ref data 'capo))
            (set! scale (assq-ref data 'scale))
            (set! x-offset (assq-ref data 'x-offset))
            (set! y-offset (assq-ref data 'y-offset))
            (set! strings (assq-ref data 'strings))))
    (if params
        (begin
            (if (assq-ref params 'number-of-strings)
                  (set! number-of-strings (assq-ref params 'number-of-strings)))
            (if (assq-ref params 'strings)    
                  (set! strings (assq-ref params 'strings)))
            (if (assq-ref params 'scale)    
                  (set! scale (assq-ref params 'scale)))                  
            (if (assq-ref params 'capo)    
                  (set! capo (assq-ref params 'capo)))))
   (if (not number-of-strings)
       (begin
            (set! number-of-strings (d-GetUserInput (_ "Fret Diagram") (_ "Give Number of Strings ") "6"))
            (if (and number-of-strings (string->number number-of-strings))
                (set! number-of-strings (string->number number-of-strings))
                (set! number-of-strings #f))))
   (if (and y-offset x-offset)
        (set! prefix (string-append "<>-\\tweak #'extra-offset #'(" x-offset " . " y-offset ") ")))       
   (if (and number-of-strings (< number-of-strings 10))
      (begin
       (do-capo)
       (do-scale)
       (let loop ((count number-of-strings))
            (if (> count 0)
                (begin
                    (set! strings (assq-set! strings count (get-fret count (assq-ref strings count))))
                    (loop (1- count)))))
        (set! data '())
        (if x-offset
             (set! data (assq-set! data 'x-offset x-offset)))
        (if y-offset
             (set! data (assq-set! data 'y-offset y-offset)))
        (set! data (assq-set! data 'number-of-strings number-of-strings))
        (set! data (assq-set! data 'strings strings))
        (set! data (assq-set! data 'scale scale))
        (if capo
            (set! data (assq-set! data 'capo capo)))
            
        (let ((markup (format #f "w:~s;f:1;" number-of-strings)))
                (if capo
                    (set! markup (string-append markup (format #f "c:~s-~s-~s;" (list-ref capo 0) (list-ref capo 1) (list-ref capo 2)))))
                (let loop ((count number-of-strings))
                    (if (> count 0)
                        (begin
                            (set! markup (string-append markup (number->string count) "-" (assq-ref strings count) ";"))
                            (loop (1- count)))))
                (d-Directive-standalone tag)
                (d-DirectivePut-standalone-postfix tag (format #f "-\\markup \\scale #'(~s . ~s) \\fret-diagram #~s " scale scale markup))
                (d-DirectivePut-standalone-prefix tag prefix)
                (d-DirectivePut-standalone-data tag (format #f "'~s" data))
                (d-DirectivePut-standalone-graphic tag "\nF\nDenemo\n24")
                (d-DirectivePut-standalone-minpixels tag 30)
                (d-SetSaved #f)
                (d-RefreshDisplay)))))