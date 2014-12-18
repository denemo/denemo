;;;Ornament
(let ((appending (Appending?))(tag "Ornament")(ornament #f)(params Ornament::params)(type #f)(lilypond #f)(graphic #f)(priority "0")(direction #f)(data #f))
  (if appending
        (set! appending (d-MoveCursorLeft)))
      
  (if (d-Directive-standalone? tag)
    (let ((choice #f)(action (RadioBoxMenu (cons (_ "Add Accidental/Ornament") 'add) (cons (_ "Up/Down") 'direction)(cons (_ "Re-order Accidentals/Ornaments") 'position))))
        (set! data (d-DirectiveGet-standalone-data tag))
        (if data
            (begin
                (set! data (eval-string data))
                (set! direction (assq-ref data 'direction))
                (set! ornament (assq-ref data 'ornament))
                (set! priority (assq-ref data 'priority))))     
        (case action
            ((direction)
                (set! direction #f))
            ((add)
                (if (d-MoveCursorRight)
                    (begin
                        (set! ornament #f)
                        (d-DirectivePut-standalone tag))
                    (d-WarningDialog (_ "No Note to apply ornament to")))))
        (if (or (eq? action 'add) (eq? action 'position))
            (begin
                (set! choice (RadioBoxMenu (cons (_ "Further from Staff") 'upper)(cons (_ "Closer to Staff") 'lower)))
                
                (case choice
                    ((upper)
                        (set! priority (number->string (+  (string->number priority) 7))))
                    ((lower)
                        (set! priority (number->string (- (string->number priority) 7)))))
                (if (eq? action 'position)
                    (d-WarningDialog (string-append (_ "Priority now ") priority (_ " you may need to repeat this command to get a suitable value to re-order this ornament with respect to other ornaments on the same note.")))))))
    (d-DirectivePut-standalone tag))
    
    
    (if (list? params)
        (begin
            (set! direction (assq-ref params 'direction))
            (set! ornament (assq-ref params 'ornament))
            (set! priority (assq-ref params 'priority))))
       
    (if (not ornament)
        (if (symbol? params)
            (set! ornament params)
            (set! ornament
                (RadioBoxMenu 
                    (cons (_ "Trill") 'trill)
                    (cons (_ "Sharp") 'sharp)
                    (cons (_ "Flat") 'flat)
                    (cons (_ "Natural") 'natural)
                    (cons (_ "Mordent") 'mordent)
                    (cons (_ "Turn") 'turn)
                    (cons (_ "Reverse Turn") 'reverseturn)
                    (cons (_ "Prall") 'prall)
                    (cons (_ "Up Prall") 'upprall)
                    (cons (_ "Down Prall") 'downprall)
                    (cons (_ "Prall Up") 'prallup)
                    (cons (_ "Prall Down") 'pralldown)
                    (cons (_ "Prall Prall") 'prallprall)
                    (cons (_ "Prall Mordent") 'prallmordent)
                    (cons (_ "Up Mordent") 'upmordent)
                    (cons (_ "Down Mordent") 'downmordent) 
                ))))
    (if ornament
        (begin
            (case ornament
               
                ((sharp)
                    (set! lilypond "\\markup\\tiny\\sharp ")
                    (set! graphic LG-Sharp))
                ((flat)
                    (set! lilypond "\\markup\\tiny\\flat ")
                    (set! graphic LG-Flat))
                ((natural)
                    (set! lilypond "\\markup\\tiny\\natural ")
                    (set! graphic LG-Natural))
                ((trill)
                    (set! lilypond "\\trill ")
                    (set! graphic LG-Trill))                         
                ((mordent)
                    (set! lilypond "\\mordent ")
                    (set! graphic LG-Mordent))
                ((turn)
                    (set! lilypond "\\turn ")
                    (set! graphic LG-Turn))
                ((reverseturn)
                    (set! lilypond "\\reverseturn ")
                    (set! graphic LG-ReverseTurn))
                ((prall)
                    (set! lilypond "\\prall ")
                    (set! graphic LG-Prall))
                ((upprall)
                    (set! lilypond "\\upprall ")
                    (set! graphic LG-UpPrall))
                ((prallup)
                    (set! lilypond "\\prallup ")
                    (set! graphic LG-PrallUp))
                ((downprall)
                    (set! lilypond "\\downprall ")
                    (set! graphic LG-DownPrall))  
                ((pralldown)
                    (set! lilypond "\\pralldown ")
                    (set! graphic LG-PrallDown))  
               ((prallmordent)
                    (set! lilypond "\\prallmordent ")
                    (set! graphic LG-PrallMordent))
               ((prallprall)
                    (set! lilypond "\\prallprall ")
                    (set! graphic LG-PrallPrall))
                ((upmordent)
                    (set! lilypond "\\upmordent ")
                    (set! graphic LG-UpMordent))
                ((downmordent)
                    (set! lilypond "\\downmordent ")
                    (set! graphic LG-DownMordent))
                    
                    
                (else
                    (AllowOrnament ornament)
                    (if (not priority)
                        (set! priority "0"))
                    (set! lilypond (assq-ref params 'lilypond))
                    (if (not lilypond)
                        (set! lilypond (string-append "\\" ornament " ")))
                    (set! graphic (assq-ref params 'graphic))
                    (if (not graphic)
                        (set! graphic ornament))))
                    
             (if (not direction)
                    (set! direction (RadioBoxMenu
                                            (cons (_ "Above Note") "^")
                                            (cons (_ "Below Note") "_")
                                            (cons (_ "Auto Position") "-"))))
            (if (not direction)
                (set! direction "-"))
                            
            (d-DirectivePut-standalone-minpixels tag 20)
            (d-DirectivePut-standalone-gx tag 20)
            (d-DirectivePut-standalone-gy tag  (if (equal? direction "_") (+ 60 (- (string->number priority) 30))  (- -30 (string->number priority))))   
            
            (d-DirectivePut-standalone-prefix tag (string-append "<>-\\tweak outside-staff-priority " priority))
            (set! data (assq-set! '() 'priority priority))
            (set! data (assq-set! data 'direction direction))
            (set! data (assq-set! data 'ornament ornament))   
           
            (d-DirectivePut-standalone-data tag (format #f "'~s" data))
            (d-DirectivePut-standalone-postfix tag (string-append direction lilypond))
            (d-DirectivePut-standalone-graphic tag graphic)

            (if appending
                (d-MoveCursorRight))
            (d-SetSaved #f)
            (d-RefreshDisplay))
        (begin
           (d-WarningDialog (_ "Cancelled")))))
