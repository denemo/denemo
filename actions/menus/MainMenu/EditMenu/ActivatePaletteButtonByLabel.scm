(if (not DenemoKeypressActivatedCommand)
	(d-InfoDialog (_ "Select a palette and use a keyboard shortcut to invoke this command and type in a label from the palette"))
	(d-ActivatePaletteButton))