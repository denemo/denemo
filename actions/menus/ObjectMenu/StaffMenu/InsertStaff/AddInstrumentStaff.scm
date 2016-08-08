;;AddInstrumentStaff
(let (  (num-staffs (d-GetStaffsInMovement))
        (key (d-InitialKey "query=keysigname"))
        (time (d-InitialTimeSig "query=timesigname"))
        (staff (d-GetStaff)))
        
    (d-AddStaffs DENEMO_INSTRUMENTS_DIR)
    (while (d-MoveToStaffDown))
    (if (> (d-GetStaff) num-staffs)
        (begin
            (while (and (> (d-GetStaff) num-staffs)
                        (d-InitialKey key)
                        (d-InitialTimeSig time)
                        (d-MoveToStaffUp)))
            (while (d-MoveToStaffDown)))
        (d-WarningDialog (_ "Cancelled"))))
    
    
    