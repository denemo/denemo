;;;ToggleArpeggio
        (ChordAnnotation "ToggleArpeggio" "\\arpeggio"    ToggleArpeggio::params    LG-Arpeggio)
				(if (d-Directive-chord? "ToggleArpeggio")
						(d-DirectivePut-chord-gx "ToggleArpeggio" -5))