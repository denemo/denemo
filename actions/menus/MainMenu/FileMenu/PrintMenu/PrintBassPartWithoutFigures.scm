;;;PrintBassPartWithoutFigures
(let ((saved (d-GetSaved)) (params PrintBassPartWithoutFigures::params))
    (if params
         (ForAllMovements "(ForAllStaffs \"
                                            (d-ShowFiguredBass)
                                            (if (d-HasFigures)
                                                (d-ShowFiguredBass)
                                                (d-NonPrintingStaff))
                                        \")")
        (begin
            (ForAllMovements  "(ForAllStaffs \"
                                                (d-ShowFiguredBass)
                                                (if (d-HasFigures)
                                                    (d-HideFiguredBass)
                                                    (d-NonPrintingStaff)) 
                                                \")")
            (d-InfoWithHook (_ "Bass figures are hidden. After printing dismiss this dialog to restore them.") "(d-PrintBassPartWithoutFigures 'finish)"))))