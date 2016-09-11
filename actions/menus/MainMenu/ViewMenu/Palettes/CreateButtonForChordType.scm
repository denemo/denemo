;;;CreateButtonForChordType
(define-once CreateButtonForChord::palette #f)
(let ((notes (d-GetNotes))(interval #f)(cursorNote #f)(root-note (GetLowestNote)))
 (if root-note
    (begin
         (if (not CreateButtonForChord::palette)
            (set! CreateButtonForChord::palette (d-SelectPalette #f)))
         (if  CreateButtonForChord::palette 
             (let ((label (d-GetUserInput (_ "Create Palette Button") (_ "Give a name for the chord") notes)))
                (if label
             		(d-CreatePaletteButton CreateButtonForChord::palette label "Inserts chord" (string-append 
                		"(DenemoInsertChordTransposed \"" notes "\" '" root-note ")"))
                	 (d-WarningDialog (_ "Cancelled"))))	
            (d-WarningDialog (_ "Cancelled"))))
    (d-WarningDialog (_ "Not on a chord"))))
