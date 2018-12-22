;;; CreateIndex
(use-modules (ice-9 ftw))
(let ((tag "IndexEntry")(params CreateIndex::params)(data #f)(count 0)(total-files 0)(Start-time (current-time)))
   (define (create-lilypond) 
        (if data
            (let ((thefile #f)(transpose #f)(title #f)(composer #f)(comment #f)(incipit #f)(instruments #f))
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! comment (assq-ref data 'comment))
                (set! incipit (assq-ref data 'incipit))
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
      (define (theproc filename statinfo flag) ;(disp "searching file " filename "\nwith flag " flag "\n")
      	   (define status -1)
           (d-KeepAlive)
           (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)))
                (let ((scmfile (string-append DenemoUserDataDir file-name-separator-string DenemoIndexEntryFile)))
                     (set! status  (system* 
                            (string-append DENEMO_BIN_DIR  "/denemo")
                            "-n" 
                            "-a" 
                            (string-append "(CreateIndexEntry \"" filename "\")")
                            ;filename))
                            "NON-EXISTANT-FILE"))
                    (if (zero? status)
                            (let ((port (open-file scmfile "r"))) 
                                (if port
                                    (begin 
                                        (set! data (read port))
                                        (close-port port)
                                        (d-SetSaved #f)                                        
                                        (set! DenemoIndexEntries (cons data DenemoIndexEntries))
                                        (d-DirectivePut-movementcontrol-postfix tag (string-append (d-DirectiveGet-movementcontrol-postfix tag) (create-lilypond))))
                                    (disp "\n\n\nFatal error " scmfile "not opened for read\n\n\n"))))))
                           
            #t); continue traversal
;;;;actual procedure   
  (define startdir (d-ChooseDirectory (_ "Where to search") DENEMO_HOME_DIR '() ))  
  (define enable_thumbnails DenemoPref_enable_thumbnails)
  (define opensources DenemoPref_opensources)
  (define ignorescripts DenemoPref_ignorescripts)
  (define autosave DenemoPref_autosave)
  (define maxhistory DenemoPref_maxhistory)
  
  (disp "Executing Create Index with parameter " params "\n\n")
  
  (d-New)
  ;(d-AppendSchemeText (string-append "(set! DenemoIndexEntries (d-DirectiveGet-movementcontrol-data \"" tag "\"))"))
  (d-NonPrintingStaff 'set)
  (d-DirectivePut-standalone-display "Info" (_ "The Index to scores will appear in the Print View."))
  (d-WriteStatus (string-append "<span font-desc=\"20\" foreground=\"red\"weight=\"bold\">" (_ "Note that the display will be very sluggish until indexing completes.") "</span>"))
  (d-SetLinesInStaff 1)
   
  (d-DirectivePut-movementcontrol-postfix (string-append tag "Header") (string-append "\\markup \\bold \\huge\\center-column{\\line{Index of Music in \\italic {" startdir 
        "}} \\column {\\draw-hline}}"))
  (d-DirectivePut-movementcontrol-postfix tag "\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}") ;you cannot store an empty string here - it is read back as #f
  (set! DenemoIndexEntries '())
  (d-SetPrefs "<enable_thumbnails>0</enable_thumbnails>")
  (d-SetPrefs "<opensources>0</opensources>")
  (d-SetPrefs "<ignorescripts>1</ignorescripts>")
  (d-SetPrefs "<autosave>0</autosave>")
  (d-SetPrefs "<maxhistory>0</maxhistory>")
  (ftw startdir theproc)
  (TimedNotice (string-append (_ "Indexing Complete in ") (number->string (- (current-time) Start-time)) " seconds."))
  (d-SetPrefs (string-append "<enable_thumbnails>" (if enable_thumbnails "1" "0") "</enable_thumbnails>"))
  (d-SetPrefs (string-append "<opensources>" (if opensources "1" "0") "</opensources>"))
  (d-SetPrefs (string-append "<ignorescripts>" (if ignorescripts "1" "0") "</ignorescripts>"))
  (d-SetPrefs (string-append "<autosave>" (if autosave "1" "0") "</autosave>"))
  (d-SetPrefs (string-append "<maxhistory>" (number->string maxhistory) "</maxhistory>"))
  (d-DirectivePut-movementcontrol-postfix tag (string-append 
                  (d-DirectiveGet-movementcontrol-postfix tag) 
                  "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{" 
                  (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))
  (d-DirectivePut-movementcontrol-data (string-append tag "StartDir") startdir)
  (d-SetSaved #f)
  (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries))) 
