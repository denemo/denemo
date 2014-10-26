;;;SetFonts
(let ((tag "SetFonts") (params SetFonts::params)(data #f)
    (values '()))
    (define (set-values)
        (define serif (assq-ref values 'serif))
        (define sans (assq-ref values 'sans))
        (define mono (assq-ref values 'mono))
        (d-DirectivePut-paper-data tag (format #f "'~s" values))
        (disp "Have " sans " and " serif "\n\n")
        (d-DirectivePut-paper-postfix tag (string-append 
                         "\n#(define fonts
        (make-pango-font-tree \"" serif "\"
                          \""sans"\"
                          \""mono"\"
                          (/ staff-height pt 20)))\n")))
    (set! data (d-DirectiveGet-paper-data tag))
    (if data
            (set! values (eval-string data))
            (begin
                (set! values (assq-set! values 'serif "Times New Roman"))
                (set! values (assq-set! values 'sans "Nimbus Sans"))                
                (set! values (assq-set! values 'mono "Luxi Mono"))))             
                          
    (if params
        (let ((serif (assq-ref params 'serif))
                (sans (assq-ref params 'sans))
                (mono (assq-ref params 'mono)))
                (set! params #f)
                (if serif
                    (set! values (assq-set! values 'serif serif)))
                (if sans
                    (set! values (assq-set! values 'sans sans)))
                (if mono
                    (set! values (assq-set! values 'mono mono))))
        (let ((choice  (RadioBoxMenu
                  (cons (_ "Titles, Lyrics etc.")   'serif)   
                  (cons (_ "Chord Names, etc. (sans serif)")   'sans)   
                  (cons (_ "Mono-spaced font") 'mono))))
                    (if choice
                      (let ((font (d-SelectFont)))
                            (if font   
                                (set! values (assq-set! values choice font))
                                (set! params 'abort)))         
                      (set! params 'abort))))
    (if (not params)
        (begin
            (d-SetSaved #f)
            (set-values)
            (d-SetSaved #f))))
