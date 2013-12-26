;;;ChangeTo6
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
        (d-Change6))
    (if shift
        (d-MoveCursorRight)))
