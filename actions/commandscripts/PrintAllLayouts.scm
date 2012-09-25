;;;PrintAllLayouts
(if (not (defined? 'PrintAllLayouts::Finished))
  (define PrintAllLayouts::Finished #f))
  
(if PrintAllLayouts::Finished
  (begin
    (if (d-SelectNextLayout)
      (begin
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)"))
      (begin
	(set!  PrintAllLayouts::Finished #f)
	(set! FinalizePrint DefaultFinalizePrint))))
  (begin
    (if (d-SelectFirstLayout)
      (begin
	(set! FinalizePrint (lambda ()  (set! PrintAllLayouts::Finished #t) (d-PrintAllLayouts)))
	(set! FinalizeTypesetting  (lambda ()  (set! FinalizeTypesetting DefaultFinalizeTypesetting)(d-PrintTypesetPDF)))	
	(d-TypesetForScript "(d-PrintView)")
	)
      (begin
	(d-WarningDialog (_ "No Layouts Visible - check View->Score Layout"))))))
