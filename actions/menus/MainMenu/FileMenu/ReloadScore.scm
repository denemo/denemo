;;;;;ReloadScore
(let ((name (d-GetFilename)))
         (if name
            (if (d-GetSaved)    
            (d-Open name)
            (begin
                (d-WarningDialog (_ "Score not saved"))
                
                (case (RadioBoxMenu (cons (_ "Discard Changes") 'discard) (cons (_ "Make a copy first\n(re-issue this Reload command afterwards!)") 'copy) (cons (_ "Cancel") 'cancel))
                   ((discard)
                        (d-SetSaved #t)    
                        (d-Open name))
                   ((copy) 
                        (d-SetSaved #t)
                        (d-SaveCopy)))))
           (d-WarningDialog (_ "Score not saved to disk"))))