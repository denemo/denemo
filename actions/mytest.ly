
\version "2.13.4"
% automatically converted from C:\Users\Guest\Desktop\elite.mxml

\header {
    encodingsoftware = "Finale 2011 for Windows"
    tagline = "Finale 2011 for Windows"
    encodingdate = "2010-07-01"
    composer = "Scott Joplin"
    title = "Elite Syncopations"
    }

#(set-global-staff-size 20.2356)
\paper {
    paper-width = 22.87\cm
    paper-height = 30.47\cm
    top-margin = 2.08\cm
    botton-margin = 2.17\cm
    left-margin = 1.71\cm
    right-margin = 1.71\cm
    between-system-space = 2.6\cm
    page-top-space = 1.81\cm
    }
\layout {
    \context { \Score
        autoBeaming = ##f
        }
    }
PartPOneVoiceOne =  \relative c''' {
    \clef "treble" \key f \major \time 2/4 | % 1
    c16 ^\markup{ \bold {Not fast.} } [ a16 g16 f16 ~ ] f16 [ a16 g16 f16
    ] | % 2
    g16 [ f16 d16 c16 ~ ] c16 [ f,16 g16 a16 ] | % 3
    c,16 [ g'8 f16 ] e8 [ g8 ] | % 4
    <e g c>8 r16 c16 r16 d16 [ r16 e16 ] \bar "||"
    \break \repeat volta 2 {
        | % 5
        r16 f16 [ d'16 <f, a c>16 ~ ] ~ ~ <f a c>16 [ d'16 <dis, a' c>8
        ] | % 6
        r16 e16 [ d'16 <e, bes' c>16 ~ ] ~ ~ <e bes' c>16 [ d'16 <e,
            bes' c>8 ] | % 7
        r16 <f a c>16 [ r16 <a c>16 ] r16 <as ces>16 [ r16 <g bes>16 ] | % 8
        a16 [ c16 d16 f16 ] s4 \break | % 9
        <a, f' a>16 [ gis'16 a16 c16 ~ ] c16 [ a16 g16 f16 ] |
        \barNumberCheck #10
        g16 [ f16 d16 f16 ~ ] f16 [ d16 c16 a16 ] | % 11
        g16 [ g'16 f16 e16 ] d16 [ a8 b16 ] | % 12
        c8 r8 <c e c'>8 r8 \break | % 13
        r16 f,16 [ d'16 <f, a c>16 ~ ] ~ ~ <f a c>16 [ d'16 <dis, a' c>8
        ] | % 14
        r16 e16 [ d'16 <e, bes' c>16 ~ ] ~ ~ <e bes' c>16 [ d'16 <e,
            bes' c>8 ] | % 15
        r16 <f a c>16 [ r16 <a c>16 ] r16 <as ces>16 [ r16 <g bes>16 ] | % 16
        a16 [ c16 d16 f16 ] s4 \break | % 17
        <a, f' a>16 [ gis'16 a16 c16 ~ ] c16 [ a16 g16 f16 ] | % 18
        g16 [ f16 d16 c16 ~ ] c16 [ d16 a16 g16 ] | % 19
        <b, f'>16 [ g'16 <b, f' a>8 ] <bes e g>16 [ <bes e g>8 <bes c>16
        ] }
    \alternative { {
            | \barNumberCheck #20
            <a f'>8 r16 c16 f16 \rest d16 [ e16 \rest e16 ] }
        {
            | % 21
            <a, f'>8 c'16 [ c16 ] c8 [ c8 ] }
        } \bar "||"
    \pageBreak \repeat volta 2 {
        | % 22
        e16 [ d16 c16 bes16 ] a16 [ bes8 g16 ] | % 23
        f16 [ g16 a16 c,16 ~ ] c16 [ a'16 g16 f16 ] | % 24
        e16 [ f16 g16 c,16 ~ ] c16 [ bes'16 a16 g16 ] | % 25
        f16 [ a16 d16 c16 ~ ] c8 [ c8 ] | % 26
        e16 [ d16 c16 bes16 ] a16 [ bes8 e,16 ] \break | % 27
        f16 [ e16 f16 a16 ~ ] a16 [ a16 g16 f16 ] | % 28
        e16 [ c'16 b16 a16 ] gis16 [ b8 e,16 ] | % 29
        a8 r8 <e bes' c>4 | \barNumberCheck #30
        e'16 [ d16 c16 bes16 ] a16 [ bes8 g16 ] | % 31
        f16 [ g16 a16 c,16 ~ ] c16 [ a'16 g16 f16 ] \break | % 32
        e16 [ f16 g16 c,16 ~ ] c16 [ bes'16 a16 g16 ] | % 33
        f16 [ a16 d16 c16 ~ ] c8 [ c8 ] | % 34
        <bes d>16 [ g'8 <bes, d>16 ~ ] ~ <bes d>16 [ g'16 <bes, d>8 ] | % 35
        <a c>16 [ f'8 <a, c>16 ~ ] ~ <a c>16 [ f'16 <a, c>8 ] | % 36
        bes16 [ g16 d'16 c16 ~ ] c16 [ bes16 e,8 ] \break }
    \alternative { {
            | % 37
            f8 [ c'16 _\markup{ \italic {repeat 8va} } c16 ] c8 [ c8 ] }
        {
            | % 38
            f,8 g16 \rest c,16 f16 \rest d16 [ e16 \rest e16 ] }
        } \bar "||"
    r16 f16 [ d'16 <f, a c>16 ~ ] ~ ~ <f a c>16 [ d'16 <dis, a' c>8 ] |
    \barNumberCheck #40
    r16 e16 [ d'16 <e, bes' c>16 ~ ] ~ ~ <e bes' c>16 [ d'16 <e, bes' c>8
    ] | % 41
    r16 <f a c>16 [ r16 <a c>16 ] r16 <as ces>16 [ r16 <g bes>16 ]
    \break | % 42
    a16 [ c16 d16 f16 ] s4 | % 43
    <a, f' a>16 [ gis'16 a16 c16 ~ ] c16 [ a16 g16 f16 ] | % 44
    g16 [ f16 d16 f16 ~ ] f16 [ d16 c16 a16 ] | % 45
    g16 [ g'16 f16 e16 ] d16 [ a8 b16 ] | % 46
    c8 r8 <c e c'>8 r8 \pageBreak | % 47
    r16 f,16 [ d'16 <f, a c>16 ~ ] ~ ~ <f a c>16 [ d'16 <dis, a' c>8 ] | % 48
    r16 e16 [ d'16 <e, bes' c>16 ~ ] ~ ~ <e bes' c>16 [ d'16 <e, bes' c>8
    ] | % 49
    r16 <f a c>16 [ r16 <a c>16 ] r16 <as ces>16 [ r16 <g bes>16 ] |
    \barNumberCheck #50
    a16 [ c16 d16 f16 ] s4 \break | % 51
    <a, f' a>16 [ gis'16 a16 c16 ~ ] c16 [ a16 g16 f16 ] | % 52
    g16 [ f16 d16 c16 ~ ] c16 [ d16 a16 g16 ] | % 53
    <b, f'>16 [ g'16 <b, f' a>8 ] <bes e g>16 [ <bes e g>8 <bes c>16 ] | % 54
    <a f'>8 r8 <a' es' f>4 \bar "||"
    \break \repeat volta 2 {
        | % 55
        \key bes \major <bes d>16 [ g'16 f16 es16 ] d16 [ c16 bes16 d16
        ] | % 56
        c16 [ bes16 g16 f16 ~ ] f16 [ bes'8 g16 ] | % 57
        f16 [ g16 f16 d16 ] bes16 [ c16 d16 <a c>16 ~ ] ~ | % 58
        <a c>4. f8 \break | % 59
        d'16 [ g16 f16 es16 ] d16 [ es16 d16 c16 ] | \barNumberCheck #60
        bes16 [ a16 g16 bes16 ~ ] bes16 [ d16 c16 bes16 ] | % 61
        a16 [ d16 f16 a16 ] g16 [ f8 e16 ] | % 62
        d8 r8 <a es' f>4 \break | % 63
        <bes d>16 [ g'16 f16 es16 ] d16 [ c16 bes16 d16 ] | % 64
        c16 [ bes16 g16 f16 ~ ] f16 [ bes'8 g16 ] | % 65
        f16 [ g16 f16 d16 ] bes16 [ c16 d16 <a c>16 ~ ] ~ | % 66
        <a c>4. f8 \pageBreak | % 67
        d'16 [ c16 d16 es16 ] f16 [ e16 f16 fis16 ] | % 68
        g16 [ fis16 g16 bes16 ~ ] bes16 [ bes16 a16 g16 ] | % 69
        f16 [ d'16 c16 f,16 ] g16 [ es16 c16 a16 ] }
    \alternative { {
            | \barNumberCheck #70
            bes8 [ e,16 f16 ] g16 [ a16 bes16 c16 ] }
        {
            | % 71
            bes4. r16 <bes bes'>16 ~ ~ }
        } \bar "||"
    \break \repeat volta 2 {
        | % 72
        <bes bes'>16 [ es16 <c c'>16 es16 ] g16 [ <bes, bes'>8 es16 ] | % 73
        <c c'>16 [ es16 g16 <bes, bes'>16 ~ ] ~ <bes bes'>16 [ <c c'>8
        <bes bes'>16 ] | % 74
        <f f'>16 [ bes16 <g g'>16 bes16 ] d16 [ <f, f'>8 bes16 ] | % 75
        <g g'>16 [ bes16 d16 <f, f'>16 ~ ] ~ <f f'>16 [ <g g'>8 bes16 ]
        \break | % 76
        <f f'>16 [ a16 <g g'>16 a16 ] c16 [ <f, f'>8 a16 ] | % 77
        <g g'>16 [ a16 c16 <f, f'>16 ~ ] ~ <f f'>16 [ <g g'>8 a16 ] | % 78
        <f f'>16 [ bes16 <g g'>16 bes16 ] d16 [ <f, f'>8 bes16 ] | % 79
        <g g'>16 [ bes16 d16 <f, f'>16 ~ ] ~ <f f'>16 [ <f f'>8 <bes
            bes'>16 ~ ] ~ \break | \barNumberCheck #80
        <bes bes'>16 [ es16 <c c'>16 es16 ] g16 [ <bes, bes'>8 es16 ] | % 81
        <c c'>16 [ es16 g16 <bes, bes'>16 ~ ] ~ <bes bes'>16 [ <c c'>8
        <bes bes'>16 ] | % 82
        <f f'>16 [ bes16 <g g'>16 bes16 ] d16 [ <f, f'>8 bes16 ] | % 83
        <g g'>16 [ bes16 d16 <f, f'>16 ~ ] ~ <f f'>16 [ <f f'>8 <e des'>16
        ~ ] ~ \break | % 84
        <e des'>16 [ bes'16 <e, des'>16 bes'16 ] c16 [ <e, des'>8 bes'16
        ] | % 85
        <f d'>16 [ bes16 c16 <f, d'>16 ~ ] ~ <f d'>16 [ <f d'>8 <f f'>16
        ~ ] ~ | % 86
        <f f'>16 [ c'16 <g g'>16 c16 ] es16 [ <a, a'>8 <bes bes'>16 ~ ]
        ~ }
    \alternative { {
            | % 87
            <bes bes'>4 r8. <bes bes'>16 ~ ~ }
        {
            | % 88
            <bes bes'>4 <bes d bes'>8 r8 }
        } }

PartPOneVoiceThree =  \relative c {
    \clef "bass" \key f \major \time 2/4 s1 | % 3
    c16 [ g'8 f16 ] e8 [ g8 ] | % 4
    c8 <c,, c'>8 [ <d d'>8 <e e'>8 ] \bar "||"
    \break \repeat volta 2 {
        | % 5
        <f f'>4. <fis fis'>8 | % 6
        <g g'>4. <gis gis'>8 | % 7
        <a a'>8 [ <es' es'>8 ] <d d'>8 [ <des des'>8 ] | % 8
        <c c'>8 [ <b b'>8 ] <bes bes'>8 [ <c, c'>8 ] \break | % 9
        f8 [ <a' c f>8 ] a,8 [ <a' c f>8 ] | \barNumberCheck #10
        bes,8 [ <bes' d f>8 ] a,8 [ <a' c f>8 ] | % 11
        b,8 [ <g' b f'>8 ] g,8 [ <g' b f'>8 ] | % 12
        <c e>8 r8 <c, c'>8 [ <c, c'>8 ] \break | % 13
        <f f'>4. <fis fis'>8 | % 14
        <g g'>4. <gis gis'>8 | % 15
        <a a'>8 [ <es' es'>8 ] <d d'>8 [ <des des'>8 ] | % 16
        <c c'>8 [ <b b'>8 ] <bes bes'>8 [ <c, c'>8 ] \break | % 17
        f8 [ <a' c f>8 ] a,8 [ <a' c f>8 ] | % 18
        bes,8 [ <bes' d f>8 ] a,8 [ <a' c f>8 ] | % 19
        <d,, d'>8 [ <d d'>8 ] <c c'>8 [ <e e'>8 ] }
    \alternative { {
            | \barNumberCheck #20
            <f f'>8 <c c'>8 [ <d d'>8 <e e'>8 ] }
        {
            | % 21
            <f f'>8 r8 r4 }
        } \bar "||"
    \pageBreak \repeat volta 2 {
        | % 22
        g'8 [ <bes c e>8 ] c,8 [ <bes' c e>8 ] | % 23
        f8 [ <a c>8 ] c,8 [ <a' c>8 ] | % 24
        g8 [ <bes c>8 ] c,8 [ <bes' c>8 ] | % 25
        f8 [ <a c>8 ] a8 [ <c f>8 ] | % 26
        g8 [ <bes c e>8 ] e,8 [ <g bes cis>8 ] \break | % 27
        d8 [ <f a d>8 ] d8 [ <f a d>8 ] | % 28
        e8 [ <a c>8 ] e8 [ <gis d'>8 ] | % 29
        <a c>8 r8 <c, c'>4 | \barNumberCheck #30
        g'8 [ <bes c e>8 ] c,8 [ <c' e>8 ] | % 31
        f,8 [ <a c>8 ] c,8 [ <a' c>8 ] \break | % 32
        g8 [ <bes c>8 ] c,8 [ <bes' c>8 ] | % 33
        f8 [ <a c>8 ] c,8 [ <a' c>8 ] | % 34
        bes,8 [ <g' bes d>8 ] bes,8 [ <g' bes d>8 ] | % 35
        c,8 [ <a' c>8 ] c,8 [ <a' c>8 ] | % 36
        c,8 [ <bes' c e>8 ] c,8 [ <g' bes c>8 ] \break }
    \alternative { {
            | % 37
            <f a c>8 r8 r4 }
        {
            | % 38
            <f a c>8 <c, c'>8 [ <d d'>8 <e e'>8 ] }
        } \bar "||"
    <f f'>4. <fis fis'>8 | \barNumberCheck #40
    <g g'>4. <gis gis'>8 | % 41
    <a a'>8 [ <es' es'>8 ] <d d'>8 [ <des des'>8 ] \break | % 42
    <c c'>8 [ <b b'>8 ] <bes bes'>8 [ <c, c'>8 ] | % 43
    f8 [ <a' c f>8 ] a,8 [ <a' c f>8 ] | % 44
    bes,8 [ <bes' d f>8 ] a,8 [ <a' c f>8 ] | % 45
    b,8 [ <g' b f'>8 ] g,8 [ <g' b f'>8 ] | % 46
    <c e>8 r8 <c, c'>8 [ <c, c'>8 ] \pageBreak | % 47
    <f f'>4. <fis fis'>8 | % 48
    <g g'>4. <gis gis'>8 | % 49
    <a a'>8 [ <es' es'>8 ] <d d'>8 [ <des des'>8 ] | \barNumberCheck #50
    <c c'>8 [ <b b'>8 ] <bes bes'>8 [ <c, c'>8 ] \break | % 51
    f8 [ <a' c f>8 ] a,8 [ <a' c f>8 ] | % 52
    bes,8 [ <bes' d f>8 ] a,8 [ <a' c f>8 ] | % 53
    <d,, d'>8 [ <d d'>8 ] <c c'>8 [ <e e'>8 ] | % 54
    <f f'>8 r8 <f f'>4 \bar "||"
    \break \repeat volta 2 {
        | % 55
        \key bes \major bes8 [ <f' bes d>8 ] d8 [ <f bes d>8 ] | % 56
        es8 [ <g bes es>8 ] d8 [ <f bes d>8 ] | % 57
        bes,8 [ <f' bes d>8 ] c8 [ <bes' c e>8 ] | % 58
        <f a es'>8 [ <f, f'>8 ] <g g'>8 [ <a a'>8 ] \break | % 59
        <bes bes'>8 [ <f' bes d>8 ] d8 [ <fis c' d>8 ] | \barNumberCheck
        #60
        g8 [ <bes d>8 ] g8 [ <bes d>8 ] | % 61
        a8 [ <d f>8 ] a8 [ <cis g'>8 ] | % 62
        <d f>8 r8 <f,, f'>4 \break | % 63
        <bes, bes'>8 [ <f'' bes d>8 ] d8 [ <f bes d>8 ] | % 64
        es8 [ <g bes es>8 ] d8 [ <f bes d>8 ] | % 65
        bes,8 [ <f' bes d>8 ] c8 [ <bes' c e>8 ] | % 66
        <f a es'>8 [ <f, f'>8 ] <g g'>8 [ <a a'>8 ] \pageBreak | % 67
        <bes bes'>8 [ <bes' d>8 ] d,8 [ <f bes d>8 ] | % 68
        es8 [ <g bes es>8 ] e8 [ <g bes cis>8 ] | % 69
        f8 [ <bes d>8 ] f,8 [ <f' a es'>8 ] }
    \alternative { {
            | \barNumberCheck #70
            <bes d>8 r8 r4 }
        {
            | % 71
            d4. s8 }
        } \bar "||"
    \break \repeat volta 2 {
        | % 72
        <es,, es'>8 [ <g' bes es>8 ] <g bes es>8 [ <d, d'>8 ] | % 73
        <es es'>8 [ <f f'>8 ] <g g'>8 [ <a a'>8 ] | % 74
        <bes bes'>8 [ <f' bes d>8 ] <f bes d>8 [ <cis cis'>8 ] | % 75
        <d d'>8 [ <f bes d>8 ] <f bes d>8 [ <f, f'>8 ] \break | % 76
        <c' c'>8 [ <f a es'>8 ] <f a es'>8 [ <f, f'>8 ] | % 77
        <c' c'>8 [ <f a es'>8 ] <c c'>8 [ <cis cis'>8 ] | % 78
        <d d'>8 [ <f bes d>8 ] <f bes d>8 [ <f, f'>8 ] | % 79
        <bes bes'>8 [ <f' bes d>8 ] <f bes d>8 [ <d, d'>8 ] \break |
        \barNumberCheck #80
        <es es'>8 [ <g' bes es>8 ] <g bes es>8 [ <d, d'>8 ] | % 81
        <es es'>8 [ <f f'>8 ] <g g'>8 [ <a a'>8 ] | % 82
        <bes bes'>8 [ <f' bes d>8 ] f,8 [ <f' bes d>8 ] | % 83
        bes,8 [ <f' bes d>8 ] f,8 [ <f' bes d>8 ] \break | % 84
        <ges, ges'>8 [ <des' des'>8 ] <bes bes'>8 [ <ges ges'>8 ] | % 85
        <f f'>8 [ <d' d'>8 ] <bes bes'>8 [ <f f'>8 ] | % 86
        <c' c'>8 [ <f a es'>8 ] <f, f'>8 [ <f' a es'>8 ] }
    \alternative { {
            | % 87
            d'4 <c,, c'>8 [ <d d'>8 ] }
        {
            | % 88
            bes''8 [ <f, f'>8 ] s4 }
        } }

PartPOneVoiceTwo =  \relative c'' {
    \repeat volta 2 {
        \clef "treble" \key f \major \time 2/4 | % 1
        c16 ^\markup{ \bold {Not fast.} } [ a16 g16 f16 ~ ] f16 [ a16 g16
        f16 ] | % 2
        g16 [ f16 d16 c16 ~ ] c16 [ \change Staff="2" f,16 g16 a16 ] s1
        \bar "||"
        \break \repeat volta 2 {
            \change Staff="1" s1. | % 8
            f'8 [ as8 ] <g c e>8 [ <bes e g>8 ] \break s1*2 \break s1. | % 16
            f8 [ as8 ] <g c e>8 [ <bes e g>8 ] \break s1. }
        \alternative { {
                s2 }
            {
                s2 }
            } \bar "||"
        \pageBreak \repeat volta 2 {
            s2*5 \break s2*5 \break s2*5 \break }
        \alternative { {
                s8 s4. _\markup{ \italic {repeat 8va} } }
            {
                s2 }
            } \bar "||"
        s1. \break | % 42
        f8 [ as8 ] <g c e>8 [ <bes e g>8 ] s1*2 \pageBreak s1. |
        \barNumberCheck #50
        f8 [ as8 ] <g c e>8 [ <bes e g>8 ] \break s1*2 \bar "||"
        \break \repeat volta 2 {
            | % 55
            \key bes \major s1*2 \break s1*2 \break s1*2 \pageBreak s1.
            }
        \alternative { {
                s2 }
            {
                s2 }
            } \bar "||"
        \break \repeat volta 2 {
            s1*2 \break s1*2 \break s1*2 \break s1. }
        \alternative { {
                s4 }
            } s4 }
    \alternative { {
            s2 }
        } }

PartPOneVoiceFour =  \relative bes {
    \repeat volta 2 {
        \clef "bass" \key f \major \time 2/4 s1*2 \bar "||"
        \break \repeat volta 2 {
            s1*2 \break s1*2 \break s1*2 \break s1. }
        \alternative { {
                s2 }
            {
                s2 }
            } \bar "||"
        \pageBreak \repeat volta 2 {
            s2*5 \break s2*5 \break s2*5 \break }
        \alternative { {
                s2 }
            {
                s2 }
            } \bar "||"
        s1. \break s2*5 \pageBreak s1*2 \break s1*2 \bar "||"
        \break \repeat volta 2 {
            | % 55
            \key bes \major s1*2 \break s1*2 \break s1*2 \pageBreak s1.
            }
        \alternative { {
                s2 }
            {
                | % 71
                bes8 [ <bes,, bes'>8 ] <c c'>8 [ <d d'>8 ] }
            } \bar "||"
        \break \repeat volta 2 {
            s1*2 \break s1*2 \break s1*2 \break s1. }
        \alternative { {
                | % 87
                bes''8 [ <bes,, bes'>8 ] }
            } s4 }
    \alternative { {
            | % 88
            d''4 <bes,, bes'>8 e8 \rest }
        } }


% The score definition
\new PianoStaff <<
    \set PianoStaff.instrumentName = "Piano"
    \context Staff = "1" << 
        \context Voice = "PartPOneVoiceOne" { \voiceOne \PartPOneVoiceOne }
        \context Voice = "PartPOneVoiceTwo" { \voiceTwo \PartPOneVoiceTwo }
        >> \context Staff = "2" <<
        \context Voice = "PartPOneVoiceThree" { \voiceOne \PartPOneVoiceThree }
        \context Voice = "PartPOneVoiceFour" { \voiceTwo \PartPOneVoiceFour }
        >>
    >>

