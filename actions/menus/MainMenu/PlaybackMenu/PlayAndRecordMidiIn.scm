;;PlayAndRecordMidiIn
    (define-once PlayAndRecordMidiIn::position #f)
   
    (if (d-AudioIsPlaying)
        (d-Stop)
        (begin
            (let ((position (GetPosition)))
                (define success #f)
                (set! PlayAndRecordMidiIn::position position)
                (while (d-MoveToStaffUp))
                (if  (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
                    (begin
                        (d-InitialTimeSig)
                        (d-CreateClickStaffForMidi)
                        (set! success  (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
                        (if success
                            (d-GoToPosition #f  (+ 1 (list-ref position 1)) (list-ref position 2)  (list-ref position 3))))
                    (begin
                        (set! success #t)
                        (d-GoToPosition #f (list-ref position 1) (list-ref position 2)  (list-ref position 3))))  
                (if success  
                    (begin
                        (d-MoveToMeasureLeft)
                        (d-MidiRecord "(d-ComputeMidiNoteDurations)
                                    (d-FirstNoteOnset)
                                    (while (d-MoveToStaffUp))
                                    (d-CursorToMarkedMidiNotePosition)
                                    (d-MoveToStaffDown)
                                    (if (not (None?)) (begin (while (d-NextObjectInMeasure))(d-MoveCursorRight)))"))))))
                        

