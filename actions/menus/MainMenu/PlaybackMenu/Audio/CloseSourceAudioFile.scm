;;;CloseSourceAudioFile
(if (d-CloseSourceAudio)
  (begin
    (d-InfoDialog "The source audio is now dropped from this score.\nIt will not automatically be re-opened with the score if you save.")
    (d-SetSaved #f))
  (d-InfoDialog "There is no source audio for this movement.\nUse File->Open->Open Source Audio to attach audio to the movement."))
