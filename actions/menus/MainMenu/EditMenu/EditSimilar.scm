;;EditSimilar
(let ((target #f))
   (define (get-tag-list get-command) ;;;get-command is d-DirectiveGetNthTag-note
    (let loop ((tags '())(n 0))
        (define tag #f)
        (set! tag (get-command n))
        (if tag
            (begin 
                (set! tags (cons tag tags))
                (loop tags (+ 1 n)))
            tags)))
        
  (define (select-directive type)
    (let ((tags '()))
        (case type
            ((note)
                (set! tags (get-tag-list d-DirectiveGetNthTagStrictNote)))
            ((chord)
                (set! tags (get-tag-list d-DirectiveGetNthTag-chord)))) 
    (if (null? tags)
            #f
        (if (> (length tags) 1)
            (RadioBoxMenuList tags)
            (list-ref tags 0)))))
  
 (define (edit-tag tag default-action)
    (let ((command (with-input-from-string (string-append "d-"  tag) read)))
        (if (defined? command)
            ((eval command (current-module)) "edit")
            (default-action tag))))
  (define (edit)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " Directives"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-standalone target))
            ((edit) (d-EditObject))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-standalone  target))
            ((#f)  (set! target #f))))

  (define (edit-note)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Noteheads"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-note target))
            ((edit) (edit-tag target d-DirectiveTextEdit-note))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-note target))
            ((#f)  (set! target #f)))
            (d-CursorUp))

  (define (edit-chord)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Chords/Notes/Rests"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-chord target))
            ((edit) (edit-tag target d-DirectiveTextEdit-chord))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-chord target))
            ((#f)  (set! target #f))))
            
            
  (define (edit-nonprinting)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " Objects"))   'continue)   
          (cons (_ "Change to Printing")   'switch)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((switch) (if (d-Directive-chord? "WholeMeasureRest") (d-WholeMeasureRest 'printing)) (d-SetNonprinting #f))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))
  (define (edit-slurstart)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking slur start positions")   'continue)   
          (cons (_ "Delete Slur")   'delete)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((delete) (d-ToggleBeginSlur)) ;;; make this execute a slur deletion instead
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))
    (define (edit-slurend)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking slur end positions")   'continue)   
          (cons (_ "Delete Slur")   'delete)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((delete) (d-ToggleEndSlur));;; make this execute a slur deletion instead
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))
            
    (define (edit-tupletstart)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking tuplet start objects")   'continue)   
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))            
            
    (define (edit-tupletend)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking tuplet end objects")   'continue)   
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))
            
    (define (edit-timesig)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking time signature change objects")   'continue)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((edit) (d-InsertTimeSig)) ;;;better - offer to add beat structure, invisibility etc
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))    
            
    (define (edit-clef)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking clef change objects")   'continue)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((edit) (d-InsertClef)) ;;;better - offer  invisibility etc
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))                     
            
    (define (edit-keysig)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking key signature change objects")   'continue)   
          (cons (_ "Sharpen") 'sharpen)
          (cons (_ "Flatten") 'flatten)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((sharpen) (begin (d-SharpenKeysig)(edit-keysig)))
            ((flatten) (begin (d-FlattenKeysig)(edit-keysig)))
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f)))) 
            
    (define (edit-stemdirection)
        (define choice (RadioBoxMenu
          (cons (_ "Continue seeking stem direction change objects")   'continue)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)))
        (case choice
            ((stop) (set! target #f))
            ((edit) (d-VoiceSetting)) ;;;offer other options
            ((execute) (d-ExecuteScheme))
            ((#f)  (set! target #f))))              
            
     (define (edit-timesigdir)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Time Signature Change Objects"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-timesig target))
            ((edit) (edit-tag target d-DirectiveTextEdit-timesig))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-timesig target))
            ((#f)  (set! target #f))))
            
              
     (define (edit-clefdir)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Clef Change Objects"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-clef target))
            ((edit) (edit-tag target d-DirectiveTextEdit-clef))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-clef target))
            ((#f)  (set! target #f))))               
            
     (define (edit-keysigdir)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Key Change Objects"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-keysig target))
            ((edit) (edit-tag target d-DirectiveTextEdit-keysig))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-keysig target))
            ((#f)  (set! target #f))))   
              
     (define (edit-voicedir)
        (define choice (RadioBoxMenu
          (cons (string-append (_ "Continue Seeking ") "\""target"\"" (_ " on Voice Change objects"))   'continue)   
          (cons (_ "Delete")   'delete)   
          (cons (_ "Edit") 'edit)
          (cons (_ "Execute Scheme") 'execute)
          (cons (_ "Stop") 'stop)
          (cons (_ "Advanced") 'advanced)))
        (case choice
            ((delete) (d-DirectiveDelete-stemdirective target))
            ((edit) (edit-tag target d-DirectiveTextEdit-stemdirective))
            ((stop) (set! target #f))
            ((execute) (d-ExecuteScheme))
            ((advanced) (d-DirectiveTextEdit-stendirective target))
            ((#f)  (set! target #f))))   
                          
                                         
             
;;; the actual procedure
  (let ((type #f))
    (if EditSimilar::params 
        (case (cdr EditSimilar::params)
            ((standalone)
                (set! type 'standalone)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
  
            ((note)
                (set! type 'note)
                (set! target (car EditSimilar::params))
                (FindNextNoteAllColumns (lambda () (d-DirectiveGetForTagStrictNote target))))
                
             ((chord) 
                (set! type 'chord)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-chord? target))))
             ((nonprinting) 
                (set! type 'nonprinting)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-GetNonprinting target))))
                
             ((slurstart) 
                (set! type 'slurstart)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-IsSlurStart))))
                
             ((slurend) 
                (set! type 'slurend)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-IsSlurEnd))))
                   
              ((tupletstart) 
                (set! type 'tupletstart)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (TupletOpen?))))               
              ((tupletend) 
                (set! type 'tupletend)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (TupletClose?))))
                
              ((timesig) 
                (set! type 'timesig)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (Timesignature?))))                
              ((keysig) 
                (set! type 'keysig)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (Keysignature?))))                
              ((stemdirection) 
                (set! type 'stemdirection)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (StemDirective?))))                
             ((clef) 
                (set! type 'clef)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (Clef?))))                       
                
              ((voicedir) 
                (set! type 'voicedir)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-stemdirective? target))))                
              ((keysigdir) 
                (set! type 'keysigdir)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-keysig? target)))) 
              ((timesigdir) 
                (set! type 'timesigdir)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-timesig? target)))) 
               ((clefdir) 
                (set! type 'clefdir)
                (set! target (car EditSimilar::params))
                (FindNextObjectAllColumns (lambda () (d-Directive-clef? target)))) 
                        
                               
                                                  
             (else
                (disp "Not handling " EditSimilar::params " yet.")
                (set! EditSimilar::params #f)))) 
    (if (not type)
        (begin
            (set! target (d-DirectiveGetTag-standalone))
            (if target 
                (set! type 'standalone)
                (begin
                    (set! target (select-directive 'note))
                    (if target
                        (set! type 'note)
                        (begin
                            (set! target (select-directive 'chord))
                            (if target
                                (set! type 'chord))))))))
                            
     (case type
             ((standalone)
                          (edit)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-standalone? target))))
                              (edit)))
            ((note)
                          (edit-note)
                          (while (and target (FindNextNoteAllColumns (lambda () (d-DirectiveGetForTagStrictNote target))))
                              (edit-note)))
            ((chord)
                          (edit-chord)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-chord? target))))
                              (edit-chord)))
                              
            ((nonprinting)
                          (edit-nonprinting)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-GetNonprinting))))
                              (edit-nonprinting)))                  
            ((slurstart)
                          (edit-slurstart)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-IsSlurStart))))
                              (edit-slurstart))) 
                              
            ((slurend)
                          (edit-slurend)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-IsSlurEnd))))
                              (edit-slurend))) 
                              
            ((tupletstart)
                          (edit-tupletstart)
                          (while (and target (FindNextObjectAllColumns (lambda () (TupletOpen?))))
                              (edit-tupletstart))) 
            ((tupletend)
                          (edit-tupletend)
                          (while (and target (FindNextObjectAllColumns (lambda () (TupletClose?))))
                              (edit-tupletend)))                                                             
 
 
            ((timesig)
                          (edit-timesig)
                          (while (and target (FindNextObjectAllColumns (lambda () (Timesignature?))))
                              (edit-timesig)))        
            ((clef)
                          (edit-clef)
                          (while (and target (FindNextObjectAllColumns (lambda () (Clef?))))
                              (edit-clef)))                                                             
            ((keysig)
                          (edit-keysig)
                          (while (and target (FindNextObjectAllColumns (lambda () (Keysignature?))))
                              (edit-keysig)))                                                             
            ((stemdirection)
                          (edit-stemdirection)
                          (while (and target (FindNextObjectAllColumns (lambda () (StemDirective?))))
                              (edit-stemdirection)))                                                             
            ((stemdirection)
                          (edit-stemdirection)
                          (while (and target (FindNextObjectAllColumns (lambda () (StemDirective?))))
                              (edit-stemdirection)))                                                             
            ((timesigdir)
                          (edit-timesigdir)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-timesig? target))))
                              (edit-timesigdir)))            
            ((keysigdir)
                          (edit-keysigdir)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-keysig? target))))
                              (edit-keysigdir)))            
            ((voicedir)
                          (edit-voicedir)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-stemdirective? target))))
                              (edit-voicedir)))            
                        
            ((clefdir)
                          (edit-clefdir)
                          (while (and target (FindNextObjectAllColumns (lambda () (d-Directive-clef? target))))
                              (edit-clefdir)))            
                              
                                                       
            (else
                (d-InfoDialog (_ "Currently only Directives attached to noteheads, chords (including notes and rests) or standalone are supported - position the cursor on a notehead for directives on that notehead or off the noteheads for directives on a chord/note/rest, or on a standalone directive. \nAlternatively, use \"Choose, Seek &amp; Edit\" to select from possible directives in your score."))))))
