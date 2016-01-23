;;;FiguredBassExercises
 (define (FiguredBassExercises::help)
   (d-InfoDialog 
  (_ "The music below is the first of Handel's exercises for figured bass. The staff marked \"Chords\" at the top is a sample (inadequate) realization of the figures.
 When you click Start you will be asked if you want to delete this version, and when you say yes you can start creating your own.
 The screen becomes green to remind you that your MIDI controller is now expecting you to play the ticked bass note plus chord.
 The bass note expected is marked with a tick, play this note plus the chord notes. You can hold down the bass note and change chords over the same bass note.
 Note, you must distinguish enharmonic differences (e.g. D-sharp from E-flat), check that the set of accidentals showing in the MIDI-in controls suits the piece you are working on. Press shift-sharpwise or flatwise as needed.
 You can hold down the chord while going on to the next bass note so as to carry the chord over.
You can use the sustain pedal to tie the chord you are entering with the chord for the next bass note by putting the pedal down on the chord you want to tie and releasing it after going on to the next bass note.
Putting pedal down before the bass note will tie the last chord to the one you enter next. 
You can also use the sustain pedal to place a chord on a rest before a bass note (depress the pedal before striking the bass note).
 You can click Start a second time to stop, and you can re-start from any bar you like.
 You can edit, playback etc while working, but don't remove or add staffs. You can also use the pitch bend wheel to stop entering chords.
  ") 
  ))
  
(if (d-OpenExample "HandelFiguredBassExercises.denemo")
    (begin
    (let loop () (if (d-PreviousMovement) (loop)))
    (CreateButton "Help" "<span font_desc=\"22\">Help</span>")
    (d-SetDirectiveTagActionScript "Help" "(FiguredBassExercises::help)")
    (CreateButton "Start" "<span font_desc=\"22\">Start/Stop</span>")
    (d-SetDirectiveTagActionScript "Start" "(d-ChordsOverBass)")
  
    (CreateButton "Next" "<span font_desc=\"22\">Next Exercise</span>")
    (d-SetDirectiveTagActionScript "Next" "(if (not (d-NextMovement)) (d-InfoDialog \"This is the last one\"))")
    (CreateButton "Prev" "<span font_desc=\"22\">Previous Exercise</span>")
    (d-SetDirectiveTagActionScript "Prev" "(if (not (d-PreviousMovement)) (d-InfoDialog \"This is the first one\"))"))
        (d-WarningDialog (_ "You have unsaved work")))
