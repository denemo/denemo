;ToggleHideOtherStaffsKeepingClick
;Hides/Unhides other staffs in the display while keeping the MIDI track at the top showing.
(d-ToggleDisplayAllStaffs)
(d-PushPosition)
(d-GoToPosition #f 1 1 1)
(if (d-Directive-clef? DenemoClickTrack)
	(d-StaffHidden #f))
(d-PopPosition)
