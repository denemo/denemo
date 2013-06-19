;;;SetCurrentStaffAsVoice
(if
 (not (d-StaffToVoice))
 (d-WarningDialog (_ "The current staff is already a voice")))