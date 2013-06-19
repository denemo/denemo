;;;SetCurrentVoiceAsStaff
(if
 (not (d-VoiceToStaff))
 (d-WarningDialog (_ "The current staff is not voice")))