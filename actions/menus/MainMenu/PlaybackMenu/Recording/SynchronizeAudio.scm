;; SynchronizeAudio
(let ()
  (define name #f)
  (while (d-MoveToStaffUp))
  (set! name (d-StaffProperties "query=denemo_name"))
  (if (equal? name "Timing")
    (d-MoveToBeginning)
    (begin
      (d-AddBefore)
      (d-StaffProperties "denemo_name=Timing")))
  (d-StartAudioPlay #t))

