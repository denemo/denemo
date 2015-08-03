;;;PlayAllAtCursor  
(DenemoSetPlaybackStart)
(if (not (d-NextObject))
    (d-MoveCursorRight))
(DenemoSetPlaybackEnd)
(d-Play)
