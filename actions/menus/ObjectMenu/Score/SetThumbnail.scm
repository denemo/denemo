;;;ThumbnailSelection
(if (d-GetSaved)
    (begin
    (if (d-PreviousMovement)
        (d-WarningDialog (_ "Returning to the first Movement for thumbnail selection")))
    (while (d-PreviousMovement)
        (display "Seeking first movement"))
    (if (d-GoToSelectionStart)
        (let ((response "y"))
            (d-SetThumbnailSelection)
            (d-Save)
            (set! response (d-GetUserInput (_ "Thumbnail Selection Complete") (_ "Create thumbnail now?") response))
            (if (and response (equal? response "y"))
                (d-CreateThumbnail #f)))    
        (d-WarningDialog (_ "No Selection to set the thumbnail to\nCreate a selection and re-run the command."))))
    (d-WarningDialog (_ "Save the score before trying to set the thumbnail")))
        
