;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ToggleArpeggio
(ToggleChordDirective "Arpeggio" LG-Arpeggio "\\arpeggio" DENEMO_OVERRIDE_ABOVE)
(if (d-Directive-chord? "Arpeggio")
    (d-DirectivePut-chord-gx "Arpeggio" -5))