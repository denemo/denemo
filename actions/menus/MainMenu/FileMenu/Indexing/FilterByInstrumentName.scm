  ;;FilterByInstrumentName
(let ((str "")(search-instrument #f)(tag "IndexEntry")(list-of-entries '()) (thefile #f) (transpose #f) (title #f) (composer #f) (incipit #f) (instruments '()))
    (define (create-lilypond data)
        (if data
            (begin
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! incipit (assq-ref data 'incipit))
                (set! instruments (apply string-append (assq-ref data 'instruments)))
                (if (string-contains instruments search-instrument)
                    (set! str (string-append str
                        "\\markup \"" composer ": " title "\"\n"
                        "\\markup {instrumentation:"  instruments "}\n"
                        transpose "\n"
                        incipit "\n\\incipit\n"
                        "\\markup {Filename: " thefile "}\n"
                        "\\markup {\\column {\\draw-hline}}"))))
            ""))

;;;;actual procedure        
  (set! DenemoIndexEntries (d-DirectiveGet-movementcontrol-data tag)) 
  (if DenemoIndexEntries
    (begin
       (set! search-instrument (d-GetUserInput (_ "Index Filter") (_ "Give Instrument Name to filter on:") (_ "Violino")))
        (set! DenemoIndexEntries (eval-string DenemoIndexEntries))
        (map  create-lilypond DenemoIndexEntries)
        (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup \\bold\\center-column{\\line{Filtered by Instrument "
                    search-instrument 
                    "}}\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}"
                    str)))
    (d-WarningDialog (_ "Create index first"))))

