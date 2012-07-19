;;;SetCurrentStaffAsVoice
(if
 (not (d-StaffToVoice))
 (d-WarningDialog "The current staff is already a voice"))