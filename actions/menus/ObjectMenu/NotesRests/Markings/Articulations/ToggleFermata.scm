;;;ToggleFermata
        
(if (d-Directive-chord? "WholeMeasureRest")
    (ChordAnnotation "ToggleFermata" "\\fermataMarkup"    ToggleFermata::params    LG-Fermata)
    (ChordAnnotation "ToggleFermata" "\\fermata"    ToggleFermata::params    LG-Fermata))
