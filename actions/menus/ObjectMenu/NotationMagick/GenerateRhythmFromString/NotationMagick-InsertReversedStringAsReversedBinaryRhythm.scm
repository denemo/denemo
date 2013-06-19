(let ((return (NotationMagick::AskNewAsciistring)))
	(if return
		 (NotationMagick::PutBinaryStringList  (reverse (NotationMagick::ReverseStringsInList (NotationMagick::String->CharsAsBinary return))))
		 #f))