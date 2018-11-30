;;; CreateIndex
(use-modules (ice-9 ftw))
(let ((tag "IndexEntry")(data #f))
   (define (create-lilypond) 
        (if data
            (let ((thefile #f)(transpose #f)(title #f)(composer #f)(incipit #f)(instruments #f))
                (set! thefile (assq-ref data 'thefile))
                (set! transpose (assq-ref data 'transpose))
                (set! title (assq-ref data 'title))
                (set! composer (assq-ref data 'composer))
                (set! incipit (assq-ref data 'incipit))
                (set! instruments (string-join (assq-ref data 'instruments) ", "))
                (string-append 
                        "\\markup {\"" composer ": " title "\"}\n"
                        "\\noPageBreak\\markup {\"Instrumentation:" instruments "\"}\n"
                        transpose
                        incipit
                        "\n\\noPageBreak\\incipit\n"
                        "\\noPageBreak\\markup {\\with-url #'\"scheme:(d-Open \\\"" thefile "\\\")\" \"Filename: " thefile "\"}\n"
                        "\\noPageBreak\\markup {\\column {\\draw-hline}}"))
            "\\markup { BLANK ENTRY }"))
      (define (theproc filename statinfo flag) ;(disp "searching file " filename "\nwith flag " flag "\n")
      	   (define status -1)
           (d-KeepAlive)
           (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)))
                (let ((scmfile (string-append filename DenemoIndexEntrySuffix)))
                     (set! status  (system* 
                            (string-append DENEMO_BIN_DIR  "/denemo")
                            "-n" 
                            "-a" 
                            (string-append "(CreateIndexEntry \"" filename "\")")
                            filename))
                    (if (zero? status)
                    
                            (let ((port (open-file scmfile "r"))) 
                                (if port
                                    (begin 
                                        (set! data (read port))
                                        (close-port port)
                                        (d-SetSaved #f)                                        
                                        (set! DenemoIndexEntries (cons data DenemoIndexEntries))
                                        (d-DirectivePut-movementcontrol-postfix tag (string-append (d-DirectiveGet-movementcontrol-postfix tag) (create-lilypond)))
                                        (d-SetSaved #f))
                                    (disp "\n\n\nFatal error " scmfile "not opened for read\n\n\n"))))))
                           
            #t); continue traversal
;;;;actual procedure   
  (define startdir (d-ChooseDirectory (_ "Where to search") DENEMO_HOME_DIR '() ))    
  (d-New)
  ;(d-AppendSchemeText (string-append "(set! DenemoIndexEntries (d-DirectiveGet-movementcontrol-data \"" tag "\"))"))
  (d-NonPrintingStaff 'set)
  (d-DirectivePut-standalone-display "Info" (_ "The Index to scores will appear in the Print View"))
  (d-DirectivePut-movementcontrol-postfix (string-append tag "Header") (string-append "\\markup \\bold \\huge\\center-column{\\line{Index of Music in \\italic {" startdir 
        "}} \\column {\\draw-hline}}"))
  (d-DirectivePut-movementcontrol-postfix tag "\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}") ;you cannot store an empty string here - it is read back as #f
  (set! DenemoIndexEntries '())
  (ftw startdir theproc)
  (d-DirectivePut-movementcontrol-postfix tag (string-append 
                  (d-DirectiveGet-movementcontrol-postfix tag) 
                  "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{" 
                  (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))

  (d-SetSaved #f)
  (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries))) 
