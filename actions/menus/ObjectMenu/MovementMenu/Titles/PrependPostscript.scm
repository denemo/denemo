;;PrependPostscript
(let ((tag "PrependPostscript")(filename #f)(width #f)(space-below #f)(space-left #f)(params PrependPostscript::params)
    (warning (_ "Wait for your vector graphics editor to start.
It will open an SVG file of the same name, if available,
but be sure to save as encapsulated postscript (eps).
You will need to refresh the print view to see your changes.
When saving your eps it is good to save as SVG file format as well, 
as this will give better editing later.  
Quit your graphics editor before quitting this dialog
to return to work in Denemo.")))

  (define (edit)
        (define choice (RadioBoxMenu
          (cons (_ "Edit") 'edit)
          (cons (_ "Delete")   'delete)   
          (cons (_ "Advanced") 'advanced)))
          (case choice
            ((delete) (d-DirectiveDelete-movementcontrol tag)(set! params 'finished))
            ((edit) 
                 (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Edit width and position ") #f))
                    (d-EditGraphics filename #f #f)
                    (set-params)))
            ((advanced) (d-DirectiveTextEdit-movementcontrol  tag))))
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
        (if (d-Directive-movementcontrol? tag)
            (let ((data (eval-string (d-DirectiveGet-movementcontrol-data tag))))
                (set! filename  (list-ref data 0))
                (set! width (list-ref data 1))
                (set! space-below (list-ref data 2))
                (set! space-left (list-ref data 3)))))
    (if (not filename)
        (let ((name (d-GetFilename)))
            (if name
                (set! filename (string-append (d-PathFromFilename name) "//" "drawing.eps"))
                (set! filename (string-append DENEMO_HOME_DIR "//" "drawing.eps")))
            (set! width "100")
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
                    (set! filename (d-EditGraphics #f #f))
                    (if filename
                        (begin
                            (set! filename (string-append filename ".eps"))
                            (d-WarningDialog warning))))           
                
                (begin
                    (set! filename (d-ChooseFile (_ "Encapsulated Postscript File") (d-PathFromFilename filename) (list "*.eps" "*.EPS")))
                    (if filename
                        (begin
                            (set! width (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  width))
                            (set! space-below (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space below required:") space-below))
                            (set! space-left (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space to the left required:") space-left))
                            (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Use the file unedited") #f))
                                (begin
                                (d-EditGraphics filename #f)
                                 (d-WarningDialog warning))))))))))

   (if (not (eq? params 'finished))
    (if (and filename width space-below space-left)
        (begin
            (if (d-FileExists filename)
                (begin
                    (d-DirectivePut-movementcontrol-override tag DENEMO_OVERRIDE_DYNAMIC) ;;call with 'refresh to re-scale for score size change 
                    (d-DirectivePut-movementcontrol-prefix tag
                        (string-append "\\markup {\\hspace #" (scale space-left) " \\with-url #'\"scheme:(d-PrependPostscript \\\"edit\\\")\" \\epsfile #X #" (scale width) " #\"" (scheme-escape filename) "\" \\vspace #" (scale space-below) " }"))
                    (d-DirectivePut-movementcontrol-data tag (string-append "(list \"" (scheme-escape filename) "\" \"" width "\" \"" space-below "\" \"" space-left "\")")))
                    
                (begin
                    (d-WarningDialog (string-append (_ "The file ") filename (_ "does not exist\nPerhaps you saved to a different directory?")))))) 
        (begin
                    (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete prepended postscript?") (_ "n")))
                    (begin
                        (d-DirectiveDelete-movementcontrol tag)
                        (d-InfoDialog (_ "Prepended Postscript Deleted"))))))))
(d-SetSaved #f)
