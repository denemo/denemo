;;RecordAndConvert
    (define-once RecordAndConvert::position #f)
    (define DenemoClickTrack (_ "Click"))
    (let ((position (GetPosition)))
        (set! RecordAndConvert::position position)
        (while (d-MoveToStaffUp))
        (if  (not (equal? DenemoClickTrack (d-StaffProperties "query=denemo_name")))
            (begin
                (d-InitialTimeSig)
                (d-CreateClickStaffForMidi)
                (d-GoToPosition #f  (+ 1 (list-ref position 1)) (list-ref position 2)  (list-ref position 3)))
            (begin
                (d-GoToPosition #f (list-ref position 1) (list-ref position 2)  (list-ref position 3))))    
        (d-MoveToMeasureLeft)
        (d-MidiRecord "(d-ComputeMidiNoteDurations)
                        (d-FirstNoteOnset)
                        (while (d-MoveToStaffUp))
                        (d-CursorToMarkedMidiNotePosition)
                        (d-MoveToStaffDown)
                        (if (not (None?)) (begin (while (d-NextObjectInMeasure))(d-MoveCursorRight)))"))
                        

