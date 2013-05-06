;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ToggleArpeggio
        (ChordAnnotation "ToggleArpeggio" "\\arpeggio"    ToggleArpeggio::params    LG-Arpeggio)
				(if (d-Directive-chord? "ToggleArpeggio")
						(d-DirectivePut-chord-gx "ToggleArpeggio" -5))