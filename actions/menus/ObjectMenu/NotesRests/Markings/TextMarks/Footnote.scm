;;;Footnote
(let ((tag "Footnote")(params "Footnote::params")(data #f)(mark #f)(text #f)(line #f)(x 0)(y 0))
    (define (set-footnote)
        (d-Chordize #t)
        (d-DirectivePut-chord-prefix tag "\\override Score.FootnoteItem #'annotation-line = ##f ")
        (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
        (d-DirectivePut-note-data tag (format #f "'~s" data))
        (d-DirectivePut-note-prefix tag (string-append "\\footnote \\markup {" mark "} #'(" x " . " y ") \\markup { \\super {" mark "} \\teeny {" text "}} "))
        (d-DirectivePut-note-display tag (string-append (_ "Fn") "\n" mark "\n" text))
        (d-DirectivePut-note-ty tag -30) 
        (d-RefreshDisplay)
        (d-SetSaved #f))
    (define (choose-footnote)
            (set! mark (d-GetUserInputWithSnippets (_ "Footnote") (_ "Give footnote marker") mark))
            (if mark
                (begin
               	    (set! mark (car mark))
                    (set! text (d-GetUserInputWithSnippets  (_ "Footnote") (_ "Give footnote text") text))
                    (if text 
                        (begin
                       	    (set! text (car text))
                            (set! x (d-GetUserInput  (_ "Footnote Mark") (_ "Give horizontal offset for the marker") x))
                            (if (and x (string->number x))
                                (begin
                                    (set! y (d-GetUserInput  (_ "Footnote Mark") (_ "Give vertical offset for the marker") y))
                                    (if (and y (string->number y))
                                        #f
                                        (set! y #f))))))))
            (if (and mark text x y)
                (begin
                    (set! data (assq-set! data 'mark mark))
                    (set! data (assq-set! data 'text text))
                    (set! data (assq-set! data 'x x))
                    (set! data (assq-set! data 'y y))
                    (set! data (assq-set! data 'line line))
                    (set-footnote))))
    (if (Appending?)
        (d-MoveCursorLeft))
        
    (set! data (d-DirectiveGet-note-data tag))    
    (if data
        (begin
            (set! data (eval-string data))
            (set! mark (assq-ref data 'mark))
            (set! text (assq-ref data 'text))
            (set! x (assq-ref data 'x))
            (set! y (assq-ref data 'y))
            (set! line (assq-ref data 'line)))
        (begin
            (set! mark "(*)")
            (set! text "N.B.")
            (set! x "0")
            (set! y "3.5")
            (set! line #f)))
    (if (not data)
        (set! data '()))
        
    (if (or (Chord?) (Note?))
        (if (d-Directive-note? tag)
                (let ((choice (GetEditOption)))
                    (case choice
                        ((edit) (choose-footnote))
                        ((delete) (begin (d-DirectiveDelete-chord tag)(d-DirectiveDelete-note tag)))
                        ((advanced) (d-DirectiveTextEdit-note tag))))
                (let ()
                    (choose-footnote)))
            (d-InfoDialog (_ "No note or chord at cursor to attach footnote to"))))
