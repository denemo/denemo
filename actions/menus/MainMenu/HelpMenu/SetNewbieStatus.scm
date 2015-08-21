;;;ToggleNewbieStatus
(let ((status (d-SetNewbie #f)))
    (if (not status)
        (begin
            (d-SetNewbie #t)
            (d-InfoDialog (_ "Newbie status is now on - re-start Denemo to see extra tooltips")))
            (begin
                (d-ShowPalettes "Select Duration" #f)
                (d-ShowPalettes "Insert or Append Duration" #f)
                (d-ShowPalettes "Edit Duration" #f)
                (d-ShowPalettes "Change or Append Notes" #f)
                (d-InfoDialog (_ "Newbie status is now off - re-start Denemo to avoid excessive tooltips.\nNote you can also increase the time before the remaining ones appear via the Preferences\nSome Newbie Palettes have been hidden, use the View menu to choose ones to re-instate.")))))
