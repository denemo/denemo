;;;CreateClickStaff
(let (  
    (old_highlight (d-HighlightCursor #f))
    ( old_volume (d-MasterVolume))
    (number #f))
            
    (if (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name"))
            (while (d-MoveToMeasureRight)
                (if (EmptyMeasure?)
                    (begin
                        (d-MoveToMeasureLeft)
                        (d-AddDuplicateMeasure)
                        (d-MoveToMeasureLeft)
                        (d-MoveToMeasureRight)
                        (if (not (Music?))
                            (d-DeleteObject)))))
            (begin
                    (d-MoveToEnd)
                    (set! number (number->string (d-GetMeasure)))
                    (d-MoveToBeginning)
                    (d-MasterVolume 0)
                    (d-NewStructuredStaff 'initial)
                    (while (d-StaffUp))
                    (d-StaffProperties "midi_channel=9")
                    (d-NonPrintingStaff)
                    (d-StaffProperties (string-append "denemo_name=" DenemoClickTrack))
                    (d-InfoDialog (_ "Now fill this measure with the clicks required and re-run this command to populate the subsequent empty measures up to any time signature change. Then repeat for each change of time signature."))
                    (d-MuteStaff "unmute")))
    (d-MasterVolume old_volume)     
    (d-HighlightCursor old_highlight)
    (d-SetSaved #f))
       
