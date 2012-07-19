;;;SetCurrentVoiceAsStaff
(if
 (not (d-VoiceToStaff))
 (d-WarningDialog "The current staff is not voice"))