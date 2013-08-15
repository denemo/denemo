;;;;;;;PrintAllLayouts

(define-once PrintAllLayouts::Finished #f)
  
(if PrintAllLayouts::Finished
  (begin (disp "printalllayouts not #finished\n")
    (if (d-SelectNextLayout)
      (begin (disp "selected next\n")
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)"))
      (begin (disp "didn't select next")
	(set!  PrintAllLayouts::Finished #f)
	(d-SelectDefaultLayout)
	(set! FinalizePrint DefaultFinalizePrint))))
  (begin (disp "printalllayouts is #f\n")
    (if (d-SelectFirstLayout)
      (begin (disp "selected first\n")
	(set! FinalizePrint (lambda ()  (set! PrintAllLayouts::Finished #t) (d-PrintAllLayouts)))
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)")
	)
      (begin
	(d-WarningDialog (_ "No Layouts Available?"))))))
