;;; CreateIndex
;;invoke as e.g. (d-CreateIndex "(begin (while (d-StaffDown))(d-HasFigures))")       

(use-modules (ice-9 ftw))

(let ((tag "IndexEntry")(params CreateIndex::params)(data #f))
  (define DenemoIndexEntries '())
  (define (condition)
      (if params
        (eval-string params)
        #t))
  (define (create-index-entry filename)
    (let (
        (outputfile #f)
        (data #f)
        (tempfile (string-append DenemoUserDataDir file-name-separator-string "DenemoIndexEntry.ly")) 
        (transpose  (d-DirectiveGet-score-prefix "GlobalTranspose")) 
        (title #f)
        (composer #f)
        (comment (d-DirectiveGet-score-display "ScoreComment"))
        (incipit (d-DirectiveGet-scoreheader-postfix "ScoreIncipit"))
        (instruments '()))

        (define (instrument-name)
            (let ((name (d-DirectiveGet-staff-display "InstrumentName")))
                (if (not name)
                    (set! name (d-StaffProperties "query=denemo_name")))
                (if name
                   (string-delete #\" name)
                   "Unknown")))
 
        (let ((data (d-DirectiveGet-scoreheader-data "ScoreTitles")));;are there simple titles? FIXME can there be both? 
            (if data
                (begin
                    (set! data (eval-string data))
                    (set! title (assq-ref data 'title))
                    (set! composer (assq-ref data 'composer)))))
                
        (if (not title)
            (begin
                (set! title (d-DirectiveGet-scoreheader-data "BookTitle"))
                (if (not title)
                    (begin
                        (set! title (d-DirectiveGet-scoreheader-display "BookTitle"))
                        (if (not title)
                           (begin
                                (d-GoToPosition 1 1 1 1)
                                (set! title (d-DirectiveGet-scoreheader-display "Title"))
                                (if (not title)
                                    (begin
                                        (set! title (d-DirectiveGet-header-display "ScoreTitle"))
                                            (if (not title)
                                                (begin
                                                    (set! title (d-DirectiveGet-scoreheader-display "ScoreTitle"))))
                                                    (if (not title)
                                                        (begin
                                                            (set! title (d-DirectiveGet-header-display "Movement-title"))))))))))))   
 
        (if (not composer)
            (begin
                (set! composer (d-DirectiveGet-scoreheader-data "BookComposer"))
                (if (not composer)
                    (begin
                        (set! composer (d-DirectiveGet-scoreheader-display "BookComposer"))
                        (if (not composer)
                                (begin
                                    (d-GoToPosition 1 1 1 1)
                                    (set! composer (d-DirectiveGet-scoreheader-display "Composer"))
                                    (if (not composer)
                                        (begin
                                            (set! composer (d-DirectiveGet-header-display "Movement-composer"))
                                            (if (not composer)
                                                (begin
                                                    (set! composer (d-DirectiveGet-header-display "ScoreComposer"))
                                                    (if (not composer)
                                                        (begin
                                                            (set! composer (d-DirectiveGet-scoreheader-display "ScoreComposer")))))))))))))) 
            
       (if (and title (string-prefix? "Score Title: " title))
            (set! title (substring title (string-length "Score Title: "))))
            
       (if (and title (string-prefix? "title: " title))
            (set! title (substring title (string-length "title: "))))
            
            
        (if (and composer (string-prefix? "composer: " composer))
            (set! composer (substring composer (string-length "composer: "))))
            
        (if (and composer (string-prefix? "Score Composer: " composer))
            (set! composer (substring composer (string-length "Score Composer: "))))
        (if (not comment)
                (set! comment ""))
                            
        (if (not transpose)
            (set! transpose "DenemoGlobalTranspose = #(define-music-function (parser location arg)(ly:music?) #{\\transpose c c#arg #}) "))

        (if (not incipit)
            (begin
                (d-RefreshLilyPond)
                (d-GoToPosition 1 1 1 1)
                (d-UnsetMark) 
                (d-IncipitFromSelection)
                (set! incipit (d-DirectiveGet-scoreheader-postfix "ScoreIncipit"))))

         (if outputfile 
          (let ((port (open-file tempfile "w"))) (disp "Testing the incipit")
            (format port "~A" (string-append 
                    transpose
                    incipit "\n\\incipit\n"))
            (close-port port)
            (if (not (zero? (system* "lilypond" "-l" "NONE" "-dno-print-pages" tempfile)))
                 (set! incipit "incipit = \\markup {No Incipit Available}"))))
                 
                 
                        
        (if (not title)
            (set! title (_ "No Title")))
        (if (not composer)
            (set! composer (_ "No Composer")))
            
        (set! title (regexp-substitute/global #f "\"" title 'pre 'post))
        (set! composer (regexp-substitute/global #f "\"" composer 'pre 'post))
    
        (set! title (scheme-escape title))
        (set! composer (scheme-escape composer))

        (while (d-MoveToStaffUp))
        (let loop ()
            (if (d-IsVoice)
                (begin
                    (if (d-MoveToStaffDown)
                        (loop)))
                (begin
                    (set! instruments (cons* (instrument-name) instruments))
                    (if (d-MoveToStaffDown)
                        (loop)))))

        (set! data (assq-set! data 'thefile filename))
        (set! data (assq-set! data 'composer composer))
        (set! data (assq-set! data 'comment comment))
        (set! data (assq-set! data 'title title))
        (set! data (assq-set! data 'transpose transpose))
        (set! data (assq-set! data 'incipit incipit))
        (set! data (assq-set! data 'instruments (reverse instruments)))
        (let ((port (if outputfile (open-file outputfile "w") #f)))
            (if port 
                (begin
                    (format port "~s" data)
                    (close-port port)
                    (d-Quit "0"))
               (begin
                    (d-SetSaved #t)
                    (format port "~s" data))))))




  (define (create-lilypond) 
        (if data
            (let ((thefile #f)(transpose #f)(title #f)(composer #f)(comment #f)(incipit #f)(instruments #f)); (disp "data is " data "\n\n")
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! comment (assq-ref data 'comment))
                (set! incipit (assq-ref data 'incipit)) ;(disp "\nInstruments is " (assq-ref data 'instruments) " which is list " (list? (assq-ref data 'instruments)) "\n\n")
                (set! instruments (string-join (assq-ref data 'instruments) ", "))
                (string-append 
                        "\\markup {\"" composer ": " title "\"}\n"
                        "\\noPageBreak\\markup {\"Instrumentation:" instruments "\"}\n"
                        (if (string-null? comment) "" (string-append "\\noPageBreak\\markup\\bold\\italic {\"Comment:" comment "\"}\n"))
                        transpose
                        incipit
                        "\n\\noPageBreak\\incipit\n"
                        "\\noPageBreak\\markup {\\with-url #'\"scheme:(d-OpenNewWindow \\\"" thefile "\\\")\" \"Filename: " thefile "\"}\n"
                        "\\noPageBreak\\markup {\\column {\\draw-hline}}"))
            "\\markup { BLANK ENTRY }"))
  (define (theproc filename statinfo flag) ;(disp "considering file " filename "\nwith flag " flag "\n")
           (d-KeepAlive)
           (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)))
                (let ()
                   (set! data #f)
                   (d-NewWindow)
                   (if (d-Open filename)
                      (if (condition)
                          (set! data (eval-string (string-append "'" (create-index-entry filename))))))
                          
                  ;(disp "Data is " data "\n\n\n")
                   (d-SetSaved #t)
                   (d-Close)
                   (if data
                      (begin (disp "Creating data for file " filename "\n")
                         (set! DenemoIndexEntries (cons data DenemoIndexEntries))
                         (d-DirectivePut-movementcontrol-postfix tag (string-append 
                              (d-DirectiveGet-movementcontrol-postfix tag) (create-lilypond)))))))
            #t); continue traversal
;;;;actual procedure   
  (define startdir (d-ChooseDirectory (_ "Where to search") DENEMO_HOME_DIR '() ))  
  (define enable_thumbnails DenemoPref_enable_thumbnails)
  (define opensources DenemoPref_opensources)
  
  (disp "Executing Create Index with parameter " params " a string? " (string? params) "\n\n")
  
  (d-New)
  ;(d-AppendSchemeText (string-append "(set! DenemoIndexEntries (d-DirectiveGet-movementcontrol-data \"" tag "\"))"))
  (d-NonPrintingStaff 'set)
  (d-DirectivePut-standalone-display "Info" (_ "The Index to scores will appear in the Print View"))
  (d-DirectivePut-movementcontrol-postfix (string-append tag "Header") (string-append "\\markup \\bold \\huge\\center-column{\\line{Index of Music in \\italic {" startdir 
        "}} \\column {\\draw-hline}}"))
  (d-DirectivePut-movementcontrol-postfix tag "\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}") ;you cannot store an empty string here - it is read back as #f
  (set! DenemoIndexEntries '())
  (d-SetPrefs "<enable_thumbnails>0</enable_thumbnails>")
  (d-SetPrefs "<opensources>0</opensources>")

  (ftw startdir theproc)
  (d-SetPrefs (string-append "<enable_thumbnails>" (if enable_thumbnails "1" "0") "</enable_thumbnails>"))
  (d-SetPrefs (string-append "<opensources>" (if opensources "1" "0") "</opensources>"))

  (d-DirectivePut-movementcontrol-postfix tag (string-append 
                  (d-DirectiveGet-movementcontrol-postfix tag) 
                  "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{" 
                  (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))
  (d-DirectivePut-movementcontrol-data (string-append tag "StartDir") startdir)
  (d-SetSaved #f)
  (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries))) 
