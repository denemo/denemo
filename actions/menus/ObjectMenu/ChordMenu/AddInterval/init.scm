;Complete Script to build chords around by giving direct interval numbers instead of moving the cursor up and down manually. By Nils Gey 01/2010
;It has 4 parameters so that the menu commands all can use the same script.
;diatonicstep: Give the diatonic interval amount. 1 = Unison/Prime/Same Note
;upordown: boolean - add the note above or below?
;lowestorhighest: boolean - start from the lowest or highest note in the current chord?
;cursorright: boolean - do a step right after adding the chordnote. Important for workflow
;
;Example: (d-AddInterval 3 #t #t #t) adds a third on top of the lowest chord-note and goes to the next object

(define (d-AddInterval diatonicstep upordown lowestorhighest cursorright)

 (if lowestorhighest 
   (d-CursorToNote (d-GetLowestNote))
   (d-CursorToNote (d-GetHighestNote))
 )

;;Main Action
 (if upordown
  (begin ;Go up from here
     (d-CursorGoUp 2 diatonicstep 1) ;starts with 2 because "1" should be the same note.
     (d-AddNoteToChord))
  (begin ;Go down from here
     (d-CursorGoDown 2 diatonicstep 1) ;starts with 2 because "1" should be the same note.
     (d-AddNoteToChord))
  ); end if

;;FIXME: Remove Cursor Right at all and let the SelectSwitcher handle this.
(if cursorright (d-MoveCursorRight))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

