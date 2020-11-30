;;;OpenNthTimeBar
(let* ((tag "OpenNthTimeBar") 
        (params OpenNthTimeBar::params)
        (text (if params params "1, 2."))
        (data #f)
        (choice #f)
        (size "1")
        (bold #t)
        (italic #f))
 
   (define (create-volta size italic bold text)
        (define instring #f)
        (define result "")
        (define (nameof adigit)
            (case adigit
                ((#\0) "\"zero\"")
                ((#\1) "\"one\"")
                ((#\2) "\"two\"")
                ((#\3) "\"three\"")
                ((#\4) "\"four\"")
                ((#\5) "\"five\"")
                ((#\6) "\"six\"")
                ((#\7) "\"seven")
                ((#\8) "\"eight\"")
                ((#\9) "\"nine\"")))
                
        (let loop ((n 0))
            (if (< n (string-length text))
                (let ()
                    (define thischar (string-ref text n))
                    (if (char-set-contains? char-set:digit thischar)
                         (begin
                            (if instring
                                    (begin   ;( to balance
                                        (set! result (string-append result "\")"))
                                        (set! instring #f)))
                            (set! result (string-append result "(make-musicglyph-markup " (nameof thischar) 
                                                    ")(make-hspace-markup -0.5)")))
                        (begin
                            (if instring
                                    (set! result (string-append result (make-string 1 thischar)))
                                    (set! result (string-append result "(make-text-markup \"" (make-string 1 thischar))))  ;) to balance
                            (set! instring #t)))
                           
                    (loop (1+ n)))
                (if instring ;( to balance
                        (set! result (string-append result "\")")))))
        (string-append   "#(list (list 'volta (make-scale-markup '(" size " . " size ")"  
                                (if bold "(make-bold-markup" "")
                                (if italic "(make-italic-markup" "")
                                "(make-line-markup (list " 
                            result
                                 (if bold ")" "")
                                 (if italic ")" "")
                                 ")))))"))      

    (set! data (d-DirectiveGet-standalone-data tag))
    (if data
            (begin
                (set! data (eval-string data))
                (set! size (assq-ref data 'size))
                (set! bold (assq-ref data 'bold))
                (set! italic (assq-ref data 'italic))
                (set! text (assq-ref data 'text))))
    (if (equal? params "edit")
        (let ((choice (RadioBoxMenu 
                (cons (_ "Help") 'help) 
                (cons (_ "1st Time Bar") 'first) 
                (cons (_ "2nd Time Bar") 'second) 
                (cons (_ "Arbitrary Text") 'nth))))
            (case choice
                    ((help)
                    (set! params 'finished)
                    (d-InfoDialog (_ "This marks the start of one or more measures to be played on one or more of the repeats. There must be a later End Volta mark else nothing prints"))
                    )
                ((first)
                    (d-DirectiveDelete-standalone tag)
                    (d-OpenFirstTimeBar #f)
                    (set! params 'finished))
                ((second)
                    (d-DirectiveDelete-standalone tag)
                    (d-OpenSecondTimeBar #f)
                    (set! params 'finished))
                ((nth) (d-DirectiveDelete-standalone tag))
                (else (set! params 'finished)))))
        
    (if (equal? params "edit")
        (let ((choice (RadioBoxMenu 
                (cons (_ "Edit Text") 'text) 
                (cons (_ "Set/Unset Bold") 'bold)
                (cons (_ "Set/Unset Italic") 'italic)
                (cons (_ "Edit Size") 'size))))
            (case choice
                
                ((text)
                    (if (not text) (set! text "3 & 4"))
                    (set! text (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give text: ") text))
                   )
                ((size)
                    (if (not size) (set! size "1"))
                    (set! size (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give size: ") size)))
                ((bold)
                    (set! bold (not bold)))
                ((italic)
                    (set! italic (not italic))))))
    (if (not (eq? params 'finished))
        (begin
            (if (or (not params) (not text))
                 (begin
                    (if (not text) (set! text "3 & 4"))
                    (set! text (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give text: ") text))))
            (if text
                (begin
                    (set! data (assq-set! '() 'text text))
                    (if size
                        (set! data (assq-set! data 'size size)))
                    (if bold
                        (set! data (assq-set! data 'bold bold)))
                    (if italic
                        (set! data (assq-set! data 'italic italic)))

                
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-data tag (format #f "'~s" data))
                    (if (not size)
                        (set! size "1"))
                    (d-DirectivePut-standalone-minpixels  tag 50)
                    (d-DirectivePut-standalone-postfix tag (string-append "
                    \\set Score.repeatCommands = " (create-volta size italic bold text)))
                   
                    (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_GRAPHIC)

                    (d-DirectivePut-standalone-gx  tag 8)
                    (d-DirectivePut-standalone-gy  tag -40)
                    (d-DirectivePut-standalone-graphic tag "NthTimeBar")
                    (d-DirectivePut-standalone-display tag text)
                    (d-MoveCursorRight)
                    (d-RefreshDisplay)
                    (d-SetSaved #f))))))
					
			

