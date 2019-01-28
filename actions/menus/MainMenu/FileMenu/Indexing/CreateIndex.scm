;;; CreateIndex
(use-modules (ice-9 ftw))
(let ((tag "IndexEntry")
      (params CreateIndex::params)
      (scmfile (string-append DenemoUserDataDir file-name-separator-string DenemoIndexEntryFile))
      (data #f)
      (count 0)
      (total-files 0)
      (Start-time (current-time)))

            
      (define (theproc filename statinfo flag) ;(disp "searching file " filename "\nwith flag " flag "\n")
           (d-KeepAlive)
           (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)))
                 (system* 
                            (string-append DENEMO_BIN_DIR  "/denemo")
                            "-n" 
                            "-a" 
                            (string-append "(CreateIndexEntry \"" filename "\" \"" params "\")")
                            "NON-EXISTANT-FILE"))
            #t); continue traversal
;;;;actual procedure

  
  (define enable_thumbnails DenemoPref_enable_thumbnails)
  (define opensources DenemoPref_opensources)
  (define ignorescripts DenemoPref_ignorescripts)
  (define autosave DenemoPref_autosave)
  (define maxhistory DenemoPref_maxhistory)
  (set! DenemoIndexStartdir (d-ChooseDirectory (_ "Where to search") DENEMO_HOME_DIR '() ))  
  
  (disp "Executing Create Index with parameter " params "\n\n")
  (if (pair? params)
    (begin
      (set! DenemoIndexProtocol (car params)) ;;eg "http://www.denemo.org/~rshann/DenemoScores"
      (set! params (cdr params))))
 
      
  
  (set! params (if params (scheme-escape params) "#t"))  
  (d-New)
  
  ;(d-AppendSchemeText (string-append "(set! DenemoIndexEntries (d-DirectiveGet-movementcontrol-data \"" tag "\"))"))
  (d-NonPrintingStaff 'set)
  (d-DirectivePut-standalone-display "Info" (_ "The Index to scores will appear in the Print View."))
  (d-WriteStatus (string-append "<span font-desc=\"20\" foreground=\"red\"weight=\"bold\">" (_ "Note that the display will be very sluggish until indexing completes.") "</span>"))
  (d-SetLinesInStaff 1)
   
  (d-DirectivePut-movementcontrol-postfix (string-append tag "Header") 
    (string-append "\\markup \\bold \\huge\\center-column{\\line{Index of Music in \\with-url #'\"" (if DenemoIndexProtocol (string-append DenemoIndexProtocol "/" DenemoIndexStartdir) "") "\"  \\italic {" DenemoIndexStartdir 
        "}} \\column {\\draw-hline}}"))
  (d-DirectivePut-movementcontrol-postfix tag "\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}") ;you cannot store an empty string here - it is read back as #f
  (set! DenemoIndexEntries '())
  (let ((port (open-file scmfile "w"))) 
    (if port
      (begin 
       ; (format port "'(") ;)
        (close-port port))
      (disp "FATAL cannot truncate " scmfile "\n")))
    
    
  (d-SetPrefs "<enable_thumbnails>0</enable_thumbnails>")
  (d-SetPrefs "<opensources>0</opensources>")
  (d-SetPrefs "<ignorescripts>1</ignorescripts>")
  (d-SetPrefs "<autosave>0</autosave>")
  (d-SetPrefs "<maxhistory>0</maxhistory>")
  (ftw DenemoIndexStartdir theproc)
  (TimedNotice (string-append (_ "Indexing Complete in ") (number->string (- (current-time) Start-time)) " seconds."))
  (disp (string-append (_ "Indexing Complete in ") (number->string (- (current-time) Start-time)) " seconds."))
  (d-SetPrefs (string-append "<enable_thumbnails>" (if enable_thumbnails "1" "0") "</enable_thumbnails>"))
  (d-SetPrefs (string-append "<opensources>" (if opensources "1" "0") "</opensources>"))
  (d-SetPrefs (string-append "<ignorescripts>" (if ignorescripts "1" "0") "</ignorescripts>"))
  (d-SetPrefs (string-append "<autosave>" (if autosave "1" "0") "</autosave>"))
  (d-SetPrefs (string-append "<maxhistory>" (number->string maxhistory) "</maxhistory>"))
  
  (let ((port (open-file scmfile "r"))) 
        (if port
            (let loop () 
                (set! data (read port)) 
                (if (eof-object? data)  
                  (close-port port) 
                  (begin
                    (set! DenemoIndexEntries (cons data DenemoIndexEntries))
                    (loop))))
                (disp "\n\n\nFatal error " scmfile "not opened for read\n\n\n")))
  (if (null? DenemoIndexEntries)
    (d-WarningDialog (if params (_ "No scores found for the given condition")(_ "No scores found")))
    (begin
        (d-DirectivePut-movementcontrol-postfix tag (string-join  (map CreateLilyPondForDenemoIndexEntry DenemoIndexEntries)))
        
        (d-DirectivePut-movementcontrol-postfix tag (string-append 
                        (d-DirectiveGet-movementcontrol-postfix tag) 
                        "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{" 
                        (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))
        (d-DirectivePut-movementcontrol-data (string-append tag "StartDir") DenemoIndexStartdir)
        (d-SetSaved #f)
        (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries))))) 
