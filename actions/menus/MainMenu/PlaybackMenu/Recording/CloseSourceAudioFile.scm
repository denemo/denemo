;;;CloseSourceAudioFile
(if (d-CloseSourceAudio)
  (begin
    (d-InfoDialog "The source audio recording is deleted.")
    (d-SetSaved #f))
  (d-InfoDialog "There is no source audio recording.\nUse File->Open->Open Source Audio to attach audio to the movement or make a live recording."))
