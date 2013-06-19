(let ((return (NotationMagick::AskNewAsciistring)))
	(if return
		 (NotationMagick::PutBinaryStringList (reverse (NotationMagick::String->CharsAsBinary return)))
		#f))