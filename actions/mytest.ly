

\header {
    title = " Sonate op. XXXVII/2"
    composer = "."
  arranger = \markup {\fontsize #2.5 "Joseph Bodin de Boismortier (1682 - 1765)" }
    meter = \markup { \fontsize #3.5 " II -   Adagio"   } 
    tagline = \markup { \fontsize #2   "Created by J.J.Gerbaud using LilyPond (http://lilypond.org)" }
}

\paper{
	top-margin = 15\mm
	after-title-space = 30\mm
	paper-width = 210\mm	

	}

#(set-global-staff-size 18)
#(set-default-paper-size "a4")

global = { }
globalTempo = {
    \override Score.MetronomeMark #'transparent = ##t
		}
	
resetBarnum = \context Score \applyContext % pour la numérotation des mesures
  #(set-bar-number-visibility 2)




%% Identification
voixI =

\context Voice = "voice 1"

\relative c' { 
	 
	 \set Staff.instrumentName = \markup { \column { "Hautbois" } }
         \set Staff.midiInstrument = "Oboe"
  \override Staff.VerticalAxisGroup #'minimum-Y-extent = #'(-6 . 6)
  \override TextScript #'padding = #2.0
  \override MultiMeasureRest #'expand-limit = 1
  \once \override Staff.TimeSignature #'style = #'()
	\set Score.tempoHideNote = ##t
  	\tempo 2=50
  	\time 2/2
        \clef "treble"
        \key g \major
              b'4.\f (c8) c4.-+ b16 c
        d4. g8 d4 f
        e4. d8 c4.-+ (b16 c)   
        b2 \grace {b16 [ (c]} d2) ~
        d2 \grace {e16[ d]}  c8 (b) a (g)
        fis4  \acciaccatura e8 d4 f4.\mordent (g16 f)   
% 7
	e2 e'2 ~
	e4  a, d2 ~
	d4 g, c2 ~
	c2 b4 a4 
	\afterGrace b2 { c8} \afterGrace a2-+  {g8}
        g4 b e2 ~
%13        
	e2 dis4.-+ (cis16 dis)
	e4 b \appoggiatura b8 (c2) \mordent ~
	c2 \appoggiatura d4 b a
	gis4-+ \appoggiatura fis8 (e4) r2 
	
	R1	
	e'2. \mordent a,4
	d2. c8-+ (b)
%20 
	c2. f4
	\afterGrace b,2 {c16 [(d]} \grace {c8[ b)]} b2-+
	a2 r
	R1
	r4 g'8 (fis) e4 g
	cis,4 g'8. (a32 g) fis8. (g32 fis) e8. (fis32 e)
	dis4 b e2 ~
	\afterGrace e2 {fis16 [(g]} \grace { a [ g fis e)] } dis2-+
%28
	e2 r
	r4 b4 b a
	a g fis e
%31
	dis4 b' b a
	a g fis4.-+ (e8)
	\afterGrace e2\fermata  {e4_\markup "librement" ~ e16 [fis g a] } b2 ~
	\afterGrace b2  {b16 [c d ] } \grace { e [ fis g g,] } a2-+
	b1 \bar "|." 

	

}      
                
%% fin voix 1 ----------------------------------------------
         
voixII =
\context Voice = "voice 2"
\relative c { 
	 \set Staff.instrumentName = \markup { \column { "Basson" } }
         \set Staff.midiInstrument = "Bassoon"
  \override Staff.VerticalAxisGroup #'minimum-Y-extent = #'(-6 . 6)
  \override TextScript #'padding = #2.0
  \override MultiMeasureRest #'expand-limit = 1
  \once \override Staff.TimeSignature #'style = #'() 

  		\clef bass
                 \key g \major
          	g'4.\p  a8 a4.-+ g16  a
	b2 g ~
	g2 fis 
	g4 d g, r4
	R1
	r2 d''2 ~ 
% 7
	d2 \grace { e16 [d]} c8 (b) a (g)
	fis4-+ \appoggiatura e8 (d4) f4.\mordent (g16 f)
	e2. a4
        fis4-+ \acciaccatura  e8 (d4) g2 ~
        g2 \afterGrace fis2-+ { g8 }
% 12
	g2 r4 b4
	a4 a a g8 (fis)
	g4 e r g
	fis2 fis2-+
	e2 e'2 ~
	e4 a, d2 ~
	d4 c8-+ (b) c2 ~
	c4 b8-+ (a) b2 ~
%20
	b4 e, a2 \mordent ~
	a2 gis-+
	a4 c8 (b) a4 c
	fis,4 c'8. (d32 c) b8. (c32 b) a8. (b32 a) 
	g4 \appoggiatura fis8 (e4) b'2 ~
%25
	b4 g a2 ~
	a2 g4 fis 
	g2 \afterGrace fis2-+ { e8 }        
	e4 b' b a
	a4 g fis e
	dis4 b' b a
	a4 g fis e
	dis4 e2 dis4 
	e4 g fis  \afterGrace b,4 { b16 [c d] }
	e1_\markup "ten."
	dis1       
                  


}

%% fin voix 2 ----------------------------------------------

%% voix 3
voixIII =
\context Voice = "voice 3"
\relative c { 
	 \set Staff.instrumentName = \markup { \column { "Cello" } }
         \set Staff.midiInstrument = "Cello"
  \override Staff.VerticalAxisGroup #'minimum-Y-extent = #'(-6 . 6)
  \override TextScript #'padding = #2.0
  \override MultiMeasureRest #'expand-limit = 1
  \once \override Staff.TimeSignature #'style = #'()  
        \clef bass  
        \key g \major
        
        g'2 c,
        g'4 g, b g
        c4 b a d
        g,g b g
        c4 c e c
        d4 c b g
        c2 a
% 8
	d2 b
	c2 a
	d2 e4 c
	d2 d,
	g2 r4 g'
	fis2 b,
	e2. e4
	dis1
	d2 cis
	f2. fis4
% 18
	gis2 a
	fis2 gis
	a4 a, c d
	e2 e,
	a2 c4 a
	d2 dis
	e4 e, g e
	a2. c4
	b2 c4 a
% 27
	b1
	e,2 dis
	e4 e' d c
	b4 e d c
	b4 e d c
	b4 c a b
	e2 ^\markup \italic "(liberamente)" d
	c1
	b1
        
  
  }
 %%%%%%%%%% fin de la musique 
\score {
	
  <<
  \new StaffGroup <<
  
  {
         \override Score.BarNumber  #'break-visibility =#end-of-line-invisible
         \override Score.RehearsalMark  #'padding = #2.5
         \resetBarnum

  }
  
  \new Staff  {\voixI }

  \new Staff  {\voixII } 

%  \new Staff \with { %%colorisation de cette portée
%     \override StaffSymbol #'stencil = #(lambda (grob)
%        (let* ((staff (ly:staff-symbol::print grob))
%               (X-ext (ly:stencil-extent staff X))
%               (Y-ext (ly:stencil-extent staff Y)))
%         (set! Y-ext (cons
%            (- (car Y-ext) 0)
%            (+ (cdr Y-ext) 0)))
%         (ly:grob-set-property! grob 'layer -10)
%         (ly:stencil-add
%           (ly:make-stencil (list 'color (rgb-color 1 0.8 0.6)
%             (ly:stencil-expr (ly:round-filled-box X-ext Y-ext 0))
%           X-ext Y-ext))
%         staff)))
%  		}
%  		{ \voixII }
	
	
  \new Staff  {\voixIII } 
  
 >>
 
 >>
 \layout { }
 	
 \midi { }
}
 %%%%%%%%%%%%%%%%%%%%%%%%%
