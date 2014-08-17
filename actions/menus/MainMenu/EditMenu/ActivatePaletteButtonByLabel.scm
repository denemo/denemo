(if (not DenemoKeypressActivatedCommand)
	(d-InfoDialog (_ "Select a palette by clicking on one of its buttons.\nThereafter use a keyboard shortcut to invoke this command\nand then type any label from the palette followed by <Return>\nYou can cut the label short (it clicks the first button that matches)."))
	(d-ActivatePaletteButton))