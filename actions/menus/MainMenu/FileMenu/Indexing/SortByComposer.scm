;;;SortByComposer takes the IndexEntry data sorts them by composer (creates the IndexEntry data from the .DenemoIndex.scm files if it is not already present) and creates and index PDF FIXME take last word of composer not first.
(use-modules (ice-9 ftw))
(let ((str "")(tag "IndexEntry")(list-of-entries '()) (thefile #f) (transpose #f) (title #f) (composer #f) (incipit #f) (instruments '()))
    (define (create-lilypond data)
        (if data
            (begin
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! incipit (assq-ref data 'incipit))
                (set! instruments (assq-ref data 'instruments))
                
                (set! str (string-append str
                        "\\markup \"" composer ": " title "\"\n"
                        "\\markup {instrumentation:"  (string-join instruments ", ") "}\n"
                        transpose "\n"
                        incipit "\n\\incipit\n"
                         "\\markup {\"Filename: " thefile "\"}\n"
                        "\\markup {\\column {\\draw-hline}}")))

            "\\markup { BLANK ENTRY }"))
    (define (comparison a b)
        (define comp1 (assq-ref a 'composer))
        (define comp2 (assq-ref b 'composer))
        (string-ci< comp1 comp2))

;;;;actual procedure        

  (let ((data (d-DirectiveGet-movementcontrol-data tag)))
        (if data
         (begin
            (set! DenemoIndexEntries (eval-string data))
            (sort! DenemoIndexEntries comparison)
            (map  create-lilypond DenemoIndexEntries)
            (d-SetSaved #f)
            (d-DirectivePut-movementcontrol-postfix tag str)
            (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries)))
        (d-WarningDialog (_ "Create index first")))))
    
