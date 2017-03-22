;InsertStanzaNumber
(let ((text "1. "))
    (define (do-bold)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Bold") 'bold)  (cons (_ "Light")  'light)))
           (case choice
            ((bold)  "\\bold ")
            (else "\\medium "))))         
    (define (do-italic)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Italic") 'italic)  (cons (_ "Normal")  'normal)))
           (case choice
            ((italic)  "\\italic ")
            (else ""))))         
    
      (set! text (d-GetUserInput (_ "Stanza Number") 
                                                (_ "Give non-lyric text to insert") 
                                                "1."))
       (if text 
            (begin
                (d-InsertTextInVerse (string-append (if (zero? (d-SyllableCount)) "" " ") "\\set stanza = \\markup {"  (do-bold) (do-italic)  text "}\n")))
            (d-SetSaved #f)))
