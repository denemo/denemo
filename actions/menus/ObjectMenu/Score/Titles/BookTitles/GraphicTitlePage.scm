;;GraphicTitlePage
(let ((tag "GraphicTitlePage")
    (filename #f)(width #f)(space-below #f)(space-left #f)
    (warning (_ "Wait for your vector graphics editor to start.
It will open an SVG file of the same name, if available,
but be sure to save as encapsulated postscript (eps).
When saving your eps it is good to save as SVG file format as well, 
as this will give better editing later.  
Quit your graphics editor before quitting this dialog
to return to work in Denemo.
If you are saving to a new file you will be asked to open it later."))
    (params GraphicTitlePage::params))

  (define (edit)
        (define choice (RadioBoxMenu
          (cons (_ "Edit") 'edit)
          (cons (_ "Delete")   'delete)   
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-score tag)(set! params 'finished))
            ((edit) 
                 (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Edit width and position ") #f))
                    (d-EditGraphics filename #f)
                    (set-params)))
            ((advanced) (d-DirectiveTextEdit-score  tag))))
  (define (set-params)
            (set! width (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  width))
            (set! space-below (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space below required:") space-below))
            (set! space-left (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space to the left required:") space-left)))
            
   (define (scale val)
       (number->string (/ (* 16 (string->number val)) (string->number (d-ScoreProperties "query=fontsize")))))
   (if (list? params)
        (begin
                (set! filename  (list-ref params 0))
                (set! width (list-ref params 1))
                (set! space-below (list-ref params 2))
                (set! space-left (list-ref params 3)))
        (if (d-Directive-score? tag)
            (let ((data (eval-string (d-DirectiveGet-score-data tag))))
                (set! filename  (list-ref data 0))
                (set! width (list-ref data 1))
                (set! space-below (list-ref data 2))
                (set! space-left (list-ref data 3)))))
                
   (if (not filename)
        (let ((name (d-GetFilename)))
            (if name
                (set! filename (string-append (d-PathFromFilename name) "//" "drawing.eps"))
                (set! filename (string-append DENEMO_HOME_DIR "//" "drawing.eps")))
            (set! width "135")
            (set! space-below "14")
            (set! space-left "0")))
            
   (if (not (list? params))
    (cond
        ((equal? params "edit")
            (begin
                (edit)))
        ((equal? params 'refresh))
        ((equal? params 'finished))
        (else
            (if (RadioBoxMenu (cons (_ "Start From Template") #t)
                           (cons (_ "Choose File") #f))
                (begin
                    (set! params 'template)
                    (set! filename (d-EditGraphics #f #f))
                    (if filename
                        (begin
                            (set! filename (string-append filename ".eps"))
                            (d-WarningDialog warning)
                            (d-SetSaved #f))))           
                
                (begin
                    (set! filename (d-ChooseFile (_ "Encapsulated Postscript File") (d-PathFromFilename filename) (list "*.eps" "*.EPS")))
                    (if filename
                        (begin
                            (set-params)
                            (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Use the file unedited") #f))
                                (begin
                                    (d-EditGraphics filename #f)
                                    (d-WarningDialog warning)
                                    (d-SetSaved #f))))))))))

   (if (not (eq? params 'finished))
    (begin
    
        (if (eq? params 'template)
            (let ((tempname #f))
                (d-WarningDialog (_ "Now dismiss this dialog and select the .eps file you have just saved in the graphics editor program."))
                (set! tempname (d-ChooseFile (_ "Encapsulated Postscript File") (d-PathFromFilename filename) (list "*.eps" "*.EPS")))
                (if tempname
                    (set! filename tempname))
            ))
    
        (if (and (d-FileExists filename) width space-below space-left)
            (begin
                    (d-DirectivePut-score-override tag DENEMO_OVERRIDE_DYNAMIC) ;;call with 'refresh to re-scale for score size change 
                    (d-DirectivePut-score-prefix tag
                            (string-append "\\markup {\\hspace #" (scale space-left) " \\with-url #'\"scheme:(d-GraphicTitlePage \\\"edit\\\")\" \\epsfile #X #" (scale width) " #\"" (scheme-escape filename) "\" \\vspace #" (scale space-below) " }"))
                    (d-DirectivePut-score-data tag (string-append "(list \"" (scheme-escape filename) "\" \"" width "\" \"" space-below "\" \"" space-left "\")")))
            (let ((message (string-append (_ "The file \"") filename (_ "\"\ndoes not (yet) exist, or no longer exists.\nTypesetting will silently fail until the file exists.\nEither create the file or delete the Graphic Title Page now"))))
            
                         (d-WarningDialog message)
            
                        (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete Graphic Title Page?") (_ "n")))
                        (begin
                            (d-DirectiveDelete-score tag)
                            (d-InfoDialog (_ "Graphic Title Page Deleted")))))))))
(d-SetSaved #f)
