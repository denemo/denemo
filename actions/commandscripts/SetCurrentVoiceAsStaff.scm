;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SetCurrentVoiceAsStaff
(if
 (not (d-VoiceToStaff))
 (d-WarningDialog (_ "The current staff is not voice")))