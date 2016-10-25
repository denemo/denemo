(define (SlurToCurrentChord)
 (if (d-PrevChord)
  (begin
    (if (Note?)
      (begin ;;; there is a note before, if it is end slur then remove that, else start slur
    (if (d-IsSlurEnd)
      (begin
        (d-ToggleEndSlur))
      (begin ;;; there is a note before it is to be the start of the slurred notes      
        (d-ToggleBeginSlur)))
    (d-NextChord)
    (d-PlayMidiNote 53 80 9 10)
    (d-ToggleEndSlur))
    (begin ;;; there is a rest before
      (d-NextChord)))
    (d-MoveCursorRight))))
