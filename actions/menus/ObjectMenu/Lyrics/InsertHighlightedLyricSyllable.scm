;InsertHighlightedLyricSyllable
(let ((text #f))
    (define (do-bold)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Bold") 'bold)  (cons (_ "Light")  'light)))
           (case choice
            ((bold)  "\\bold ")
            (else ""))))         
    (define (do-italic)
        (let ()                  
          (define choice (RadioBoxMenu (cons (_ "Italic") 'italic)  (cons (_ "Normal")  'normal)))
           (case choice
            ((italic)  "\\italic ")
            (else ""))))         
    
      (set! text (d-GetUserInput (_ "Highlighted Syllable") 
                                                (_ "Give syllable to insert:") 
                                                ""))
       (if text 
            (d-InsertTextInVerse (string-append (if (zero? (d-SyllableCount)) "" " ") "\\markup "  (do-bold) (do-italic) "\n\""  text "\" "))
        (d-SetSaved #f)))
