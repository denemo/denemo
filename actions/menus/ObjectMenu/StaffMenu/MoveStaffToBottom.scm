;;;MoveStaffToBottom
(d-TakeSnapshot)
(d-IncreaseGuard)
(while (d-MoveToStaffDown)
	(d-SwapStaffs)
	(d-MoveToStaffDown))
(d-DecreaseGuard)