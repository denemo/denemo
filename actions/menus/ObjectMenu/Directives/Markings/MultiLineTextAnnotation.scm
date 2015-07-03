;;;MultiLineTextAnnotation
(let ((text #f)
        (params  MultiLineTextAnnotation::params) 
        (tag "MultiLineTextAnnotation")(markup #f)(scale #f)
        (x-offset #f)
        (y-offset #f)
        (prefix "<>")
        (direction #f)
        (dimensions "")
        (dim #f)
        (data #f))
    (define (get-text) (disp "text is " text " on entry which is " (string? text) " not " (pair? text))
        (if (string? text)
            (set! text (d-GetUserInputWithSnippets #f #f text 'format))
            (set! text (d-GetUserInputWithSnippets (_ "Text") 
                (_ "Give text to appear with following note/chord:\nThe characters \\, \", §, { and } have a special meaning in the text,\nthe backslash \\ starts some LilyPond syntax, the others must be paired. New lines will be observed unless they come between § ... § characters.\nTo apply italic or bold to a group of words enclose them in {}, e.g. \\bold {These words are bold}.\nOther markup commands \\super, \\tiny etc, see LilyPond documentation.") 
                (if text (car text) ""))))  (disp "returning text" text "\n"))
    (define (get-scale)
                    (if (not scale)
                        (set! scale "1"))
                    (set! scale (d-GetUserInput (_ "Scaling Text") (_ "Give text size: ") scale)))

     (set! data (d-DirectiveGet-standalone-data tag))
     (if data
        (set! data (eval-string data))
        (set! data '()))
    (set! dim (assq-ref data 'dimensions))    
    (set! direction (assq-ref data 'direction))    
    (set! text (assq-ref data 'text))

    (set! scale (assq-ref data 'scale))
    (set! x-offset (assq-ref data 'x-offset))
    (set! y-offset (assq-ref data 'y-offset))
    
    (if (list? params)
        (begin
            (if  (assq-ref params 'dimensions)
                (set! dim  (assq-ref params 'dimensions)))
            (if  (assq-ref params 'direction)
                (set! dim  (assq-ref params 'direction)))
        (if  (assq-ref params 'text)
                (set! text  (assq-ref params 'text)))
        (if  (assq-ref params 'scale)
                (set! scale  (assq-ref params 'scale)))                    
        (if  (assq-ref params 'x-offset)
                (set! x-offset  (assq-ref params 'x-offset)))                  
        (if  (assq-ref params 'y-offset)
                (set! y-offset  (assq-ref params 'y-offset)))
        (set! params 'finished)))              

  
    (if (and  (d-Directive-standalone? tag) text)
        (set! prefix (d-DirectiveGet-standalone-prefix tag)))
        
    (if (string? text) 
        (get-text))    ;;; get the markup
        
    (if (equal? "edit" params)
        (let ((choice (RadioBoxMenu (cons (_ "Edit Text") 'text) 
        (cons (_ "Edit Space Occupied") 'space)
        (cons (_ "Edit Size") 'scale)
        (cons (_ "Up/Down") 'position)
         (cons (_ "Edit Position") 'offset))))
            (set! params #f)
            (case choice
              ((text)
                (set! params 'finished)
                ;;;(set! text (d-GetUserInputWithSnippets #f #f text 'format))
                (get-text))
              ((scale) 
                (set! params 'finished)
                (get-scale))
              ((position)
                    (set! params 'finished)
                    (set! direction (GetLilyPondDirection)))
              ((space) 
                    (begin
                        (set! params 'finished)
                        (set! dim (d-GetUserInput (_ "Space Occupied by Text/Music") (_ "Give space:\n(0 prevents dragging position\nBlank for natural size.)") dim)))
                        (if (not (and dim (string->number dim) (>= (string->number dim) 0)))
                                    (set! dim #f)))
              ((offset) 
                    (set! params 'finished)
                    (set! x-offset (d-GetUserInput (_ "Offsets")  (_ "Give horizontal displacement required") x-offset))
                    (set! y-offset (d-GetUserInput (_ "Offsets")  (_ "Give vertical displacement required") y-offset))      
                    (if (not (and x-offset (string->number x-offset) y-offset (string->number y-offset)))
                        (begin
                            (set! x-offset #f)
                            (set! y-offset #f)))))))

    (if (and y-offset x-offset)
        (set! prefix (string-append "<>-\\tweak #'extra-offset #'(" x-offset " . " y-offset ") ")))        

    (if dim
        (set! dimensions (string-append "\\with-dimensions #'(-" dim " . " dim ") #'(-" dim " . " dim ")")))
                
    (if (not dimensions)
                (set! dimensions ""))
     (if (not direction)
        (set! direction "^"))      
    (if (string? text)
        (begin
            (set! params 'finished)
            (set! text (cons text ""))
            (get-text)))
    (if (not (eq? params 'finished))
        (get-text))
            
    (if text 
       (begin
            (if (not scale)
                (get-scale))

            (if (not scale) 
                (set! scale "1"))
            (set! markup (cdr text))
            (set! text (car text))
            (set! data (assq-set! data 'text text))
            (set! data (assq-set! data 'scale scale))
            (if dim 
                (set! data (assq-set! data 'dimensions dim))
                (set! data (assq-remove! data 'dimensions)))
       (set! data (assq-set! data 'direction direction))
            (if x-offset
             (set! data (assq-set! data 'x-offset x-offset)))
            (if y-offset
             (set! data (assq-set! data 'y-offset y-offset)))
                       
            (if (not (d-Directive-standalone? tag))
                (d-DirectivePut-standalone tag))
            (d-DirectivePut-standalone-data tag (format #f "'~s" data))
            (d-DirectivePut-standalone-display tag text)
            (d-DirectivePut-standalone-postfix tag (string-append direction "\\markup"dimensions"\\scale #'(" scale " . " scale ")\\column{" markup "}"))
            (d-DirectivePut-standalone-prefix tag prefix)
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-RefreshDisplay)
            (d-SetSaved #f))
        (begin
            (if (not params)
                (let ((confirm (d-GetUserInput (d-DirectiveGet-standalone-display tag) (_ "Delete this text?") (_ "y"))))
                 (if (and confirm (equal? confirm (_ "y")))
                    (begin
                        (d-DirectiveDelete-standalone tag)
                        (d-SetSaved #f))
                    (d-InfoDialog (_ "Cancelled"))))))))
