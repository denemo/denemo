;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SetCurrentStaffAsVoice
(if
 (not (d-StaffToVoice))
 (d-WarningDialog (_ "The current staff is already a voice")))