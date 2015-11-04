;;;SetCurrentStaffAsVoice
(if
 (not (d-StaffToVoice))
 (d-WarningDialog (_ "Current staff is already a voice or not suitable")))
