;;;;;;;ChooseSeekEditDirectives
(let ()
    (define tags '())
    (define tag #f)
    (define (get-tags)
        (d-CursorToNthNoteHeight 1)
        (let outer-loop ()
            (let loop ((n 0))
                (set! tag (d-DirectiveGetNthTagStrictNote n))
                (if tag
                    (begin
                        (set! tags (assoc-set! tags (string-append  tag " (N)") (cons tag 'note)))
                        (loop (+ 1 n)))))
                        
            (if (d-CursorToNextNoteHeight)
                        (outer-loop)))
                        
        
        (let loop ((n 0))
                (set! tag (d-DirectiveGetNthTag-chord n))
                (if tag
                    (begin
                        (set! tags (assoc-set! tags (string-append  tag " (C)") (cons tag 'chord)))
                        (loop (+ 1 n)))))
        (set! tag (d-DirectiveGetForTag-standalone))
        (if tag       
              (set! tags (assoc-set! tags (string-append  tag " (S)") (cons tag 'standalone))))) 

    ;;;actual procedure
    (d-PushPosition)   
    (let loop ((position (GetPosition)))
        (get-tags)
        (if (d-NextObject)
            (loop (GetPosition))
            (if (d-GoToPosition #f (+ 1 (list-ref position 1)) (list-ref position 2) 0)
                (begin
                    (d-MoveToBeginning)
                    (loop (GetPosition))))))
    (d-PopPosition)
    (d-MoveCursorLeft) ; to include the original position if possible.
    (if (null? tags)
        (d-InfoDialog (_ "No directives are present on any of the objects after the cursor position (searching in column order)."))
    (let ((choice (RadioBoxMenuList tags)))
       (if choice
            (d-EditSimilar choice)))))
