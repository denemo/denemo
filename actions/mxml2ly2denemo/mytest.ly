
%% LilyPond file generated by Denemo version 0.8.19

%%http://www.gnu.org/software/denemo/

%\version "2.10"

\header{
}



% The music follows

MvmntIVoiceI =  {
          c'16 <g c' e'>32 \key d \minor g' \clef french a' \bar "||"
}


MvmntIVoiceII =  {
          e4 \clef tenor g a a a a a \time 6/8 <g c' g'>8 e2 \bar ":|"
}



        MvmntIVoiceITimeSig = \time 4/4 
MvmntIVoiceIKeySig = \key c \major
 MvmntIVoiceIClef = \clef treble 
MvmntIVoiceIProlog = { \MvmntIVoiceITimeSig \MvmntIVoiceIKeySig \MvmntIVoiceIClef}
MvmntIVoiceIMusic =  {\MvmntIVoiceIProlog \MvmntIVoiceI}
MvmntIVoiceIContext = \context Voice = VoiceIMvmntI  {\MvmntIVoiceIMusic}

        MvmntIVoiceIITimeSig = \time 2/4 
MvmntIVoiceIIKeySig = \key c \major
 MvmntIVoiceIIClef = \clef bass 
MvmntIVoiceIIProlog = { \MvmntIVoiceIITimeSig \MvmntIVoiceIIKeySig \MvmntIVoiceIIClef}
MvmntIVoiceIIMusic =  {\MvmntIVoiceIIProlog \MvmntIVoiceII}
MvmntIVoiceIIContext = \context Voice = VoiceIIMvmntI  {\MvmntIVoiceIIMusic}
MvmntIStaffI = \new Staff  << {
                \MvmntIVoiceIContext
                }
                >>
MvmntIStaffII = \new Staff  << {
                \MvmntIVoiceIIContext
                }
                >>



\score {
<< <<
\MvmntIStaffI
\MvmntIStaffII
>>
>>

}

\score {
<< <<

\MvmntIStaffII
>>
>>



\layout{
        }
\header{
        }

}



