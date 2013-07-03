;Change staff-by DRW modified by RTS can take a denemo-staff name to move to
(let ((tag "ChangeStaff") (Name #f)(Choices  ""))
  (define (GetNames) 
    (let ((Dummy ""))
      (set! Dummy  (d-StaffProperties "query=denemo_name"))
      (if Dummy (set! Choices (string-append Choices Dummy stop)))
      (if (d-MoveToStaffDown) (GetNames))))
  (if (equal? ChangeStaff::params "edit")
    (set! ChangeStaff::params #f))
  (set! Name ChangeStaff::params)

  (begin
    (if (not Name)
      (begin
        (d-PushPosition)
        (while (d-MoveToStaffUp))
        (GetNames)
        (d-PopPosition)
        (set! Name (d-GetOption Choices ))))
    (if Name 
      (begin
        (d-Directive-standalone tag)
        (d-DirectivePut-standalone-minpixels  tag 50)
        (d-DirectivePut-standalone-postfix tag (string-append "\\change Staff=\"" Name "\" " ))
        (d-DirectivePut-standalone-graphic tag (string-append 
          "\n⇒" Name "\nDenemo\n12"))
        (d-DirectivePut-standalone-gy  tag -1)))
  (d-SetSaved #f)  
  (d-RefreshDisplay)))
