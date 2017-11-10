;;;ArbitraryKeySignature
(let ((tag "ArbitraryKeySignature")(accs #f)(note #f)(acc #f))
    (let loop ()
        (set! note (RadioBoxMenu
                      (cons (_ "C") "0")
                      (cons (_ "D") "1")
                      (cons (_ "E") "2")
                      (cons (_ "F") "3")
                      (cons (_ "G") "4")
                      (cons (_ "A") "5")
                      (cons (_ "B") "6")
                      
                      (cons (_ "Finish") #f)))
        (if note 
            (begin
                (set! acc (RadioBoxMenu
                  (cons (_ "Natural") ",NATURAL")
                  (cons (_ "Flat")   ",FLAT")   
                  (cons (_ "Sharp") ",SHARP")
                  (cons (_ "Double Sharp") ",DOUBLE-SHARP")
                  (cons (_ "Double Flat") ",DOUBLE-FLAT")))
                (if acc
                    (begin
                        (if (not accs)
                            (set! accs ""))
                        (set! accs (string-append accs "(" note " . " acc 
                                    ")"))
                        (loop))))))
    (if accs
        (begin
            (d-SetSaved #f)
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-graphic tag "\nK\nDenemo\n40")
            (d-DirectivePut-standalone-minpixels tag 40)
            (d-DirectivePut-standalone-postfix tag  (string-append "\\set Staff.keySignature = #`(" accs ")")))))
        