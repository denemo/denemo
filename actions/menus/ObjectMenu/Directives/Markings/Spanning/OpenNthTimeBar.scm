;;;OpenNthTimeBar
(let ((tag "OpenNthTimeBar") 
        (params OpenNthTimeBar::params)
        (text "1, 2.")
        (data #f)
        (choice #f)
        (size "1.5")
        (bold #f)
        (italic #f))
        
    (set! data (d-DirectiveGet-standalone-data tag))
    (if data
            (begin
                (set! data (eval-string data))
                (set! size (assq-ref data 'size))
                (set! bold (assq-ref data 'bold))
                (set! italic (assq-ref data 'italic))
                (set! text (assq-ref data 'text))))
        
    (if (equal? params "edit")
        (let ((choice (RadioBoxMenu (cons (_ "Help") 'help) 
                (cons (_ "Edit Text") 'text) 
                (cons (_ "Set/Unset Bold") 'bold)
                (cons (_ "Set/Unset Italic") 'italic)
                (cons (_ "Edit Size") 'size))))
            (case choice
                ((help)
                    (d-InfoDialog (_ "This marks the start of one or more measures to be played on one or more of the repeats. There must be a later End Volta mark else nothing prints"))
                    )
                ((text)
                    (set! text (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give text: ") text))
                   )
                ((size)
                    (set! size (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give size: ") size)))
                ((bold)
                    (set! bold (not bold)))
                ((italic)
                    (set! italic (not italic))))
            (set! params 'finished)))
            

    (if (or (not params) (not text))
         (set! text (d-GetUserInput (_ "Nth Time Bar Text") (_ "Give text: ") text)))
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
                (set! size "1.5"))
            (d-DirectivePut-standalone-minpixels  tag 50)
            (d-DirectivePut-standalone-postfix tag (string-append "
            \\set Score.repeatCommands = #(list (list 'volta (make-scale-markup '(" size " . " size ")" 
                (if bold "(make-bold-markup" "")
                (if italic "(make-italic-markup" "")
                 "(make-text-markup \"" text "\")"
                 (if bold ")" "")
                 (if italic ")" "")
                 ")))"))

            (d-DirectivePut-standalone-gx  tag 8)
            (d-DirectivePut-standalone-gy  tag -40)
            (d-DirectivePut-standalone-graphic tag "NthTimeBar")
            (d-DirectivePut-standalone-display tag text)
            (d-MoveCursorRight)
            (d-RefreshDisplay)
            (d-SetSaved #f))))
