;;;;ToggleTripleting
(if  ToggleTripleting::InsideTriplet
       (begin 
       		(d-PendingMidi 72)
       		(d-StartTriplet))
       	(begin
       		(d-PlayMidiNote 77 255 9 100)
      	 	(d-EndTuplet)))
(set! ToggleTripleting::InsideTriplet (not ToggleTripleting::InsideTriplet))
