;;; AddToIndex
(use-modules (ice-9 ftw))
(let ((tag "IndexEntry")
      (filename #f)
      (params #f) ;AddToIndex::params)
      (scmfile (string-append DenemoUserDataDir file-name-separator-string DenemoIndexEntryFile))
      (data #f)
      (count 0)
      (total-files 0)
      (Start-time (current-time)))

            
  (define (create-data filename)
       (if (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename))
             (system* 
                        (string-append DENEMO_BIN_DIR  "/denemo")
                        "-n" 
                        "-a" 
                        (string-append "(CreateIndexEntry \"" filename "\" \"" params "\")")
                        "NON-EXISTANT-FILE")))
;;;;actual procedure

  
  (define enable_thumbnails DenemoPref_enable_thumbnails)
  (define opensources DenemoPref_opensources)
  (define ignorescripts DenemoPref_ignorescripts)
  (define autosave DenemoPref_autosave)
  (define maxhistory DenemoPref_maxhistory)
  
  (disp "Executing Create Index with parameter " params "\n\n")
  (if (pair? params)
    (begin
      (set! DenemoIndexProtocol (car params)) ;;eg "http://www.denemo.org/~rshann/DenemoScores"
      (set! params (cdr params))))
 
      
  
  (set! params (if params (scheme-escape params) "#t"))  
  (set! DenemoIndexEntries '())
  (let ((data (d-DirectiveGet-movementcontrol-data tag)))
        (if data
         (begin
            (set! DenemoIndexStartdir (d-DirectiveGet-movementcontrol-data (string-append tag "StartDir")))
            (if (not DenemoIndexStartdir)
                (set! DenemoIndexStartdir ""))
            (set! DenemoIndexEntries (eval-string data)))))
 (if (null? DenemoIndexEntries)
    (d-WarningDialog (_ "Not an Index"))
    (begin
          (set! filename (d-ChooseFile (_ "Choose Denemo Score to add to Index"           ) DenemoIndexStartdir))

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
          (create-data filename)
          (d-SetPrefs (string-append "<enable_thumbnails>" (if enable_thumbnails "1" "0") "</enable_thumbnails>"))
          (d-SetPrefs (string-append "<opensources>" (if opensources "1" "0") "</opensources>"))
          (d-SetPrefs (string-append "<ignorescripts>" (if ignorescripts "1" "0") "</ignorescripts>"))
          (d-SetPrefs (string-append "<autosave>" (if autosave "1" "0") "</autosave>"))
          (d-SetPrefs (string-append "<maxhistory>" (number->string maxhistory) "</maxhistory>"))
          
          (let ((port (open-file scmfile "r"))) 
                (if port
                  (begin
                    (set! data (read port)) 
                    (set! DenemoIndexEntries (cons data DenemoIndexEntries))
                    (close-port port)) 
                  (disp "\n\n\nFatal error " scmfile "not opened for read\n\n\n")))


          (d-DirectivePut-movementcontrol-postfix tag (string-join  (map CreateLilyPondForDenemoIndexEntry DenemoIndexEntries)))
          
          (d-DirectivePut-movementcontrol-postfix tag (string-append 
                          (d-DirectiveGet-movementcontrol-postfix tag) 
                          "\n\\noPageBreak\\markup {\\column {\\draw-hline}}\\noPageBreak\\markup {\\center-column {\\vspace #2 }}\\noPageBreak\\markup\\huge{" 
                          (_ "End of Index. Number of entries ") (number->string (length DenemoIndexEntries)) ".}"))
          (d-DirectivePut-movementcontrol-data (string-append tag "StartDir") DenemoIndexStartdir)
          (d-SetSaved #f)
          (d-DirectivePut-movementcontrol-data tag (format #f "'~s" DenemoIndexEntries)))))
