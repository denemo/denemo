;MakeGrace
(d-ToggleGrace)
(if (d-IsGrace)
 	(d-PlayMidiNote 84 255 9 100)
 	(d-PlayMidiNote 35 255 9 100))