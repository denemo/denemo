
VoiceI = \notes \context Staff = VoiceI {

	\clef bass
	c4
}         
BassFiguresLine = \context FiguredBass
\figures {
\property FiguredBass.BassFigure \override #'font-relative-size = #-3 
}         


\score {
	<
		\VoiceI
		\BassFiguresLine
	>
	\paper {
	}

}
