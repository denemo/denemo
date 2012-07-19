
   (if (defined? 'DrumHash)
	   #t
	  (begin  (display "\nNo DrumHash existent. Force Reload!\n")    (d-ReloadDrumHash) ))
  

(define (ConvertDrumGm2UserSingleNote)
(if (and (d-GetNotes) (hashq-ref DrumHash (string->symbol (GetHighestNote))) (hashq-ref DrumHash (string->symbol (GetLowestNote))))
 
(let  ((newDrumList (string-tokenize(d-GetNotes)) ))

 (let transformChordList ((i 0))
    (if (<= i (-(length newDrumList )1)) 
       (begin
         (set! newDrumList (Replace-nth newDrumList i (hashq-ref DrumHash (string->symbol (list-ref newDrumList i)))))
          (transformChordList (+ i 1) )
        )
      )
  (d-ChangeChordNotes (string-join newDrumList)))

)

))

(if (defined? 'DrumHash)
  (SingleAndSelectionSwitcher ConvertDrumGm2UserSingleNote)
  (begin (display "No DrumHash was loaded. Please copy and use the template that comes with Denemo. Abort.\n")#f)
)
(d-RefreshDisplay)


