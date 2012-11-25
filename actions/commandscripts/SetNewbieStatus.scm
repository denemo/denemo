;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ToggleNewbieStatus
(let ((status (d-SetNewbie #f)))
	(if (not status)
		(begin
			(d-SetNewbie #t)
			(d-InfoDialog (_ "Newbie status is now on - re-start Denemo to see extra tooltips")))
			(d-InfoDialog (_ "Newbie status is now off - re-start Denemo to avoid excessive tooltips.\nNote you can also increase the time before the remaining ones appear via the Preferences"))))