(use-modules (ice-9 ftw))
(let ((tag "IndexEntry"))
      (define (theproc filename statinfo flag) ;(disp "searching file " filename "\nwith flag " flag "\n")
      	   (define status -1)
           (d-KeepAlive)
           (if (and (eq? flag 'regular) (or (string-suffix? ".denemo" filename) (string-suffix? ".denemo.gz" filename)))
                (let ()
                     (set! status  (system* 
                            (string-append DENEMO_BIN_DIR  "/denemo")
                            "-n" 
                            "-a" 
                            (string-append "(CreateIndexEntry \"" filename "\")")
                            filename))))
             
            (if (zero? status)
                (begin
                    (d-DirectivePut-movementcontrol-postfix tag (string-append (d-DirectiveGet-movementcontrol-postfix tag) "\n\\include \"" (string-append filename ".DenemoIndex.ily") "\"\n")))
                 (if (= 256 status) ; 255 is added by system, Denemo exited with 1
                    (begin
                      (d-DirectivePut-movementcontrol-postfix tag (string-append (d-DirectiveGet-movementcontrol-postfix tag) "\n\\markup {\\column {\\draw-hline}}
                          \n\\markup  \"Note:" filename " cannot be indexed\" \\markup \"try creating a self-contained incipit for it.\"\n \\markup {\\column {\\draw-hline}}
                          ")))))
            (d-SetSaved #f)
            #t); continue traversal
;;;;actual procedure        
  (define startdir (d-ChooseDirectory "Where to search" DENEMO_HOME_DIR '() ))
  (d-New)
  (d-NonPrintingStaff 'set)
  (d-DirectivePut-standalone-display "Info" (_ "The Index to scores will appear in the Print View"))
  (d-DirectivePut-movementcontrol-postfix tag (string-append "\\markup \\bold \\huge\\center-column{\\line{Index of Music in \\italic {" startdir 
        "}} \\column {\\draw-hline}}\\markup {\\column {\\draw-hline}} \\markup {\\center-column {\\vspace #2 }}   "))
  (ftw startdir theproc)) 
