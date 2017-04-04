;;NewStaffAllMovements
(let ((staff (d-GetStaff)))
    (d-PushPosition)
    (d-MoveToBeginning)
    (d-NewStructuredStaff)
    (while (d-NextMovement)
    (if (d-GoToPosition #f  staff #f #f)
        (d-NewStructuredStaff)))
    (d-PopPosition))
    