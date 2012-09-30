;;;OpenMyDefaultTemplate
(if (not (d-GetSaved))
	(d-New))
(if (d-OpenMyTemplate "Default.denemo")
		(d-SetSaved #t)
		(d-WarningDialog (_ "Failed to open Default.denemo in your custom templates directory\nHave you saved a template called Default yet?")))
