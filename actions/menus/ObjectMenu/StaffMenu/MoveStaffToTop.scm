;;;MoveStaffToTop
(d-TakeSnapshot)
(d-IncreaseGuard)
(d-SwapStaffs)
(while (> (d-GetStaff) 1)
	(d-SwapStaffs))
(d-DecreaseGuard)
