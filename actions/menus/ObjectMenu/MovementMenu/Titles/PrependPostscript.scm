;;PrependPostscript
(let ((tag "PrependPostscript")(filename #f)(width #f)(space-below #f)(space-left #f)(params PrependPostscript::params))
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
                (d-EditGraphics filename)))
        ((equal? params 'refresh))
        (else
                (set! filename (d-ChooseFile (_ "Encapsulated Postscript File") (d-PathFromFilename filename) (list "*.eps" "*.EPS")))
                (if filename
                    (begin
                        (set! width (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give width required:")  width))
                        (set! space-below (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space below required:") space-below))
                        (set! space-left (d-GetUserInput (_ "Encapsulated Postscript File") (_ "Give space to the left required:") space-left))
                        (if (RadioBoxMenu (cons (string-append (_ "Edit the file ") filename) #t) (cons (_ "Use the file unedited") #f))
                            (begin
                            (d-EditGraphics filename)
                             (d-WarningDialog (_ "Wait for your vector graphics editor to start.
It will open an SVG file of the same name, if available,
but be sure to save as encapsulated postscript (eps).
You will need to refresh the print view to see your changes.
When saving your eps it is good to save as SVG file format as well, 
as this will give better editing later.  
Quit your graphics editor before quitting this dialog
to return to work in Denemo.")))))))))

    (if (and filename width space-below space-left)
        (begin
                (d-DirectivePut-movementcontrol-prefix tag
                        (string-append "\\markup {\\hspace #" space-left " \\with-url #'\"scheme:(d-AppendPostscript 'refresh)\" \\epsfile #X #" width " #\"" filename "\" \\vspace #" space-below " }"))
                (d-DirectivePut-movementcontrol-data tag (string-append "(list \"" (scheme-escape filename) "\" \"" width "\" \"" space-below "\" \"" space-left "\")")))
        (begin
                    (if (equal? (_ "y") (d-GetUserInput  (_ "Encapsulated Postscript File") (_ "Delete prepended postscript?") (_ "n")))
                    (begin
                        (d-DirectiveDelete-movementcontrol tag)
                        (d-InfoDialog (_ "Prepended Postscript Deleted")))))))
(d-SetSaved #f)
