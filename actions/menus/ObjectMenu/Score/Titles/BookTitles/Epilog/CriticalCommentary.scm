;;CriticalCommentary
(if CriticalCommentary::params
  (begin
        (if (equal? CriticalCommentary::params "edit")
            (let ((choice #f))
                (set! choice (RadioBoxMenu
                    (cons (_ "Advanced") 'advanced)
                    (cons (_ "Delete") 'delete)))
                (case choice
                    ((advanced) 
                        (if (not (d-DirectiveTextEdit-score "CriticalCommentary"))
                            (d-DirectiveDelete-score "CriticalCommentary")))
                    ((delete) 
                        (d-DirectiveDelete-score "CriticalCommentary"))))))
  (let ((intro (d-DirectiveGet-score-display "CriticalCommentaryIntro")))
    (define Title "Critical Commentary")
    (define Prolog "Key: m = Movement, v = voice, b = bar")
    (if intro
     (begin
      (set! Title (substring  intro 0 (string-index intro #\nl)))
      (set! Prolog (substring  intro (string-index intro #\nl)))))
    (d-PushPosition)
    (while (d-PreviousMovement))

    (let ((thecomments '())  (tag "CriticalComment"))
     ;;;format-commentary numbers the strings in the list
     (define (format-commentary thelist)
      (define ret " \\markup \\vspace #1 \\markup \\column {\n")
      (let loop ((count 0))
        (if (< count (length thelist))
          (begin         
            (set! ret (string-append ret "  \\vspace #0.5 \n\\column { \\line \\large {" (number->string (+ 1 count)) ".  "
              (list-ref thelist count) "}\n"))
          (loop (+ 1 count)))))
     (string-append ret "}\n"))
   (let movement ()
    (define movement-number (number->string (d-GetMovement)))
    (define voice 1)
    (while (d-MoveToStaffUp))
    (d-MoveToBeginning)
    ;;; for each measure go through all the staffs accumulating a list of comments with the movement/staff/bar location prepended to each.
    (let measure ()
      (define measure-number (number->string (d-GetMeasure)))
      (define thecomment #f)
        (let loop ()
          (if (d-Directive-standalone? tag) 
           (begin
            (set! thecomment (d-DirectiveGet-standalone-postfix tag))
          ;  (if (equal? (string-ref thecomment 0) #\\)
             ;   (begin 
               ;     (set! thecomment (substring thecomment 19))
                ;    (set! thecomment (string-drop-right thecomment 1))))
           
            (set! thecomments (cons* (string-append (_ "At m") movement-number (_ " v")
            (number->string voice) (_ " b") measure-number ":} "
            thecomment " %end of critical comment\n") thecomments))))
        (if (NextDirectiveOfTagInMeasure tag)
          (loop)))       
      (if (or (d-MoveToVoiceDown) (d-MoveToStaffDown))
        (begin
          (set! voice (+ 1 voice))
          (measure))
        (begin
          (if (d-MoveToMeasureRight)
            (begin
              (while  (d-MoveToStaffUp))
                (set! voice 1)
                (measure))))))
                
    (if (d-NextMovement)
      (movement)
      (begin
        (d-SetSaved #f) 
        ;(d-LilyPondInclude "book-titling.ily")
        (d-LilyPondInclude "simplified-book-titling.ily")
        (d-DirectivePut-score-override "CriticalCommentary" DENEMO_OVERRIDE_AFFIX)
        (if (null? thecomments)
          (begin
            (d-DirectivePut-score-postfix "CriticalCommentary"
              (string-append "\\pageBreak\n\\titledPiece \\markup \"" (scheme-escape Title)
              "\"\n\\markup {\\vspace #1  \\fill-line {\\postscript #\"-12 3 moveto 24 0 rlineto stroke\"}}\n\\markup {\\italic \\wordwrap-string #\""
               (scheme-escape Prolog) "\"}\n" )))
          (begin
            (d-DirectivePut-score-postfix "CriticalCommentary" (string-append "\\pageBreak\n\\titledPiece \\markup \""
              (scheme-escape Title) "\"\n\\markup { \\vspace #0.8 \\fill-line {\\postscript #\"-16 3 moveto 24 0 rlineto stroke\"}}\n\\markup {\\italic \\wordwrap-string #\""
              (scheme-escape Prolog) "\" \\vspace #1 }\n"
              (format-commentary (reverse thecomments))))))))))             
   (d-PopPosition)
   (d-DirectiveDelete-score "CriticalCommentsAmended")))
 
