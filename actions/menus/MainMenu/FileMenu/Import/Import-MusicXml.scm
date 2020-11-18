(if (not (MovementEmpty?))
	(begin
		(d-NewMovement)
		(while (d-StaffUp))
		(while (d-StaffDown)
			(d-DeleteStaff))
			(d-DeleteStaff)))
(d-ImportMusicXml)
