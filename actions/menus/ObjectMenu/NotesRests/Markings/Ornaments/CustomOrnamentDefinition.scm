;;CustomOrnamentDefinition
(let ((tag "CustomOrnamentDefinition") (def-tag #f) (name #f)
    (filename #f)(width #f)
    (warning (_ "Wait for your vector graphics editor to start.
It will open an SVG file of the same name, if available,
but be sure to save as encapsulated postscript (eps),
using the name given.
You will need to refresh the print view to see your changes.
When saving your eps it is good to save as SVG file format as well, 
as this will give better editing later.  
Quit your graphics editor before quitting this dialog
to return to work in Denemo."))
    (params CustomOrnamentDefinition::params))

  (define (edit)
        (let ((choice (RadioBoxMenu
                              (cons (_ "Edit") 'edit)
                              (cons (_ "Delete")   'delete)   
                              (cons (_ "Advanced") 'advanced)))) (disp "def-tag " def-tag "\n\n")
                            (case choice
                                ((delete) (d-DirectiveDelete-score def-tag)(set! params 'finished))
                                ((edit) 
                                     (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Edit width ") #f))
                                        (d-EditGraphics filename #f)
                                        (set-params))
                                        (set! params 'referesh))
                                ((advanced) (d-DirectiveTextEdit-score  def-tag)(set! params 'finished))
                                (else (set! params 'finished)(d-WarningDialog (_ "Cancelled"))))))
                
                
        (define (set-params)
                    (set! width (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  width)))
        (define (set-def-tag name)
                        (set! def-tag (string-append "Allow\n" name)))
        (define (scale val)
               (number->string (/ (* 16 (string->number val)) (string->number (d-ScoreProperties "query=fontsize")))))
        (define (get-second-line text)
                (let ((thelist (string-split text #\newline)))
                    (if (> (length thelist) 1)
                        (list-ref thelist 1)
                        "")))       
        (define (extract-menuitem tag)
                (define orn-name (get-second-line tag))
                (cons orn-name (eval-string (d-DirectiveGet-score-data tag))))     
        (define (get-definition)
                (let ((directives '()) (definitions #f) (choice #f))
                    (set! directives (GetDefinitionDirectives))
                    (if (not (null? directives))
                        (begin
                            (set! definitions (map extract-menuitem directives))
                            (set! params (d-PopupMenu definitions)))
                        (begin
                            (d-WarningDialog (_ "No definitions created for this score"))
                            #f))))
                    
   (define (use-params)
        (set! name  (list-ref params 0))
                            (set-def-tag name) 
                            (set! filename  (list-ref params 1))
                            (set! width (list-ref params 2)))
       

    
    (if (equal? params "edit")
            (begin 
                (if (get-definition)
                    (begin
                        (if (list? params)
                            (begin
                                (use-params)
                                (edit))
                            (begin
                                (d-WarningDialog "Nothing selected?")
                                (set! params 'finished))))
                    (begin
                        (set! params 'finished)
                        (d-WarningDialog (_ "No definitions selected")))))
            (begin
               (if (list? params)
                    (use-params)

                    (begin
                            (set! name (d-GetUserInput (_ "Custom Ornament") (_ "Give Ornament Name") ""))
                            (if (string-index name char-set:digit)
                                (begin
                                    (d-WarningDialog "Numerals are not allowed in ornament names")
                                    (set! name #f)))
                            (if name
                                (begin
                                    (set! name (string-downcase name 0 1))
                                    (set-def-tag name)
                                    (if (d-Directive-score? def-tag)
                                        (let ((data (eval-string (d-DirectiveGet-score-data def-tag))))
                                            (set! filename  (list-ref data 1))
                                            (set! width (list-ref data 2)))))
                                (set! params 'finished))))                         
               (if (and (not (equal? params 'finished)) (not filename))
                    (let ((scorename (d-GetFilename)))
                        (if scorename
                            (set! filename (string-append (d-PathFromFilename scorename) "//" "drawing.eps"))
                            (set! filename (string-append DENEMO_HOME_DIR "//" "drawing.eps")))
                        (set! width "2")))               
               (if (not (list? params))
                (cond
                    ((equal? params "edit")
                        (begin (disp "this condition is redundant, edit is already done earlier!!!\n")
                            (edit)))
                    ((equal? params 'refresh))
                    ((equal? params 'finished))
                    (else
                       ; (if filename
                        ;    (d-WarningDialog (_ "This will replace the current definition")))
                    
                        (case (RadioBoxMenu (cons (_ "Start From Template") 'template) (cons (_ "Choose Custom Template") 'custom)
                                       (cons (_ "Choose File") 'choose))
                            ((template)
                                (set! filename (d-EditGraphics #f name))
                                (if filename
                                    (begin
                                        (set! filename (string-append filename ".eps"))
                                        (d-WarningDialog warning))
                                    (set! params 'finished))) 
                             ((custom)
                                (set! filename (d-ChooseFile (_ "Encapsulated Postscript File") (string-append DENEMO_LOCAL_ACTIONS_DIR "//graphics") (list "*.eps" "*.EPS")))
                                (if filename
                                    (set-params)
                                    (begin
                                        (d-WarningDialog (_ "Cancelled"))
                                        (set! params 'finished))))
                            
                            ((choose)
                                (set! filename (d-ChooseFile (_ "Encapsulated Postscript File") (d-PathFromFilename filename) (list "*.eps" "*.EPS")))
                                (if filename
                                    (begin
                                        (set-params)
                                        (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Use the file unedited") #f))
                                            (begin
                                            (d-EditGraphics filename #f)
                                             (d-WarningDialog warning))))
                                    (set! params 'finished))))))
                  (edit))))

    (if (not (eq? params 'finished))
        (if (and (d-FileExists filename) width)
            (begin
            
                    (d-CreatePaletteButton "Custom Ornaments" name (_ "Attaches (or removes) this ornament from the current note/chord.") (string-append "(if (CheckForLilyPondDefine \"" name "\")
                        (ChordOrnament \"Toggle" (string-upcase  name 0 1) "\"  \"\\\\" name "\" #f  \"" name "\") (d-WarningDialog \"Not Defined\"))"))
                    (d-LilyPondDefinition (cons name (string-append "\\tweak outside-staff-priority #50 -\\markup {\\epsfile #X #" (scale width) " #\"" (scheme-escape filename) "\" }")))
                    (d-DirectivePut-score-override def-tag (logior DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_DYNAMIC)) ;;call with 'refresh to re-scale for score size change 
                    (d-DirectivePut-score-data def-tag (string-append "(list \"" name "\" \"" (scheme-escape filename) "\" \"" width "\")")))
            (let ((message (string-append (_ "The file \"") filename (_ "\"\ndoes not (yet) exist, or no longer exists.\nTypesetting will silently fail until the file exists.\nEither create the file or delete the Graphic Title Page now"))))
            
                         (d-WarningDialog message)
            
                        (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete Custom Ornament?") (_ "n")))
                        (begin
                            (d-DirectiveDelete-score def-tag)
                            (d-InfoDialog (_ "Custom Ornament Definition Deleted"))))))))
(d-SetSaved #f)
