;;;ChangeTo0
(let ((shift #f))
    (if (Appending?)
        (begin
            (set! shift #t)
            (d-MoveCursorLeft)))
    (if (Music?)
		(d-Change0))
    (if shift
        (d-MoveCursorRight)))