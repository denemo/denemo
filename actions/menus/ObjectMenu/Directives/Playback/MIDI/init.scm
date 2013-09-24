;;;;;;;;;;;;; Midi Commands By Nils Gey 01/2010
;MenuCommand or another script call the procedure by (ChangeXXX::Set optional values) and this calls the parent function. Three steps but this avoids redundancy and makes it most easy for the user who must not be let in direct contact with midi bytes
;Either the values are given directly or the script will pop up a dialog to ask for more information which will be the cause if the command is called as menuversion.

;Parent function. Builds the executed function
(define* (ChangeMidi::Parent DirectiveName DirectiveDisplayName Bytes UserValue #:optional (Flags #f)) 

 (define (realAction)
   (d-Directive-standalone DirectiveName)
  (if Flags
   (d-DirectivePut-standalone-override DirectiveName Flags))
   (d-DirectivePut-standalone-midibytes DirectiveName (string-append Bytes " " (number->string (- (abs UserValue) 1 ))))
   (d-DirectivePut-standalone-minpixels DirectiveName 20) 
   (d-DirectivePut-standalone-display DirectiveName (string-append DirectiveDisplayName (number->string (abs UserValue) )))
   (d-DirectivePut-standalone-ty DirectiveName -20)
   (d-RefreshDisplay)
  )
  
(if  (and (number? UserValue)(> (abs UserValue) 0) )
  (realAction)
  (display (string-append "Wrong value: "UserValue ". " DirectiveName " parameter must be a non-negative number. Start with 1, not with 0.\n")))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Change the Channel of a staff
(define* (ChangeChannel::Set #:optional (UserValue (string->number (d-GetUserInput "Change Midi Channel" "Please enter a channel number. Normally 1-16" "1"))))
(define override (logior DENEMO_OVERRIDE_CHANNEL DENEMO_OVERRIDE_STEP) )
(ChangeMidi::Parent "ChannelChange" "chan" "" UserValue override)
)

;;Change the Program of the current channel/staff
(define* (ChangeProgram::Set #:optional (UserValue (string->number (d-GetUserInput "Change Midi Program Number" "Please enter a program number. Normally 1-128" "1"))))
(ChangeMidi::Parent "ProgramChange" "prog" "0xC$" UserValue)
)

;;Change the Volume of a channel/staff
(define* (ChangeVolume::Set #:optional (UserValue (string->number (d-GetUserInput "Change Midi Volume" "Please enter a volume value. Normally 1-128. 1 means off" "100"))))
(ChangeMidi::Parent "VolumeChange" "vol" "0xB$ 0x07" UserValue)
)

;;Generic 0xB Control Change - User can give the midi bytes, too. 
(define* (ChangeGeneric::Set #:optional (UserBytes (d-GetUserInput "Generic Control Change" "Please enter the controller number in hex (0x01 for modwheel) or decimal (1 for modwheel)" "0x01") ) (UserValue (string->number (d-GetUserInput "Enter Databyte Value" "Please enter the databyte pedal value. Normally 1-128." "65"))))
(ChangeMidi::Parent "GenericChange" UserBytes (string-append "0xB$ " UserBytes) UserValue)
)

;;Number 1 - Change the Modwheel value of a channel/staff
(define* (ChangeModwheel::Set #:optional (UserValue (string->number (d-GetUserInput "Change Modwheel Value" "Please enter a modwheel value. Normally 1-128. 1 means off" "100"))))
(ChangeMidi::Parent "ModwheelChange" "mod" "0xB$ 0x01" UserValue)
)

;;Number 2 - Breath Controller Todo: 14-bit coarse/fine resolution. 0x000 to 0x3FFF where 0/1 is minimum.
;(define* (ChangeVolume::Set #:optional (UserValue (string->number (d-GetUserInput "Breath Controller Value" "Please enter a breath pressure value. Normally 1-128. 1 means off" "100"))))
;(ChangeMidi::Parent "BreathControlChanger" "mod" "0xB$ 0x02" UserValue)
;)

;;Number 8 - Balance. Typically used for a stereo signal tweak without changing the pan itself. Like a CD player. 
(define* (ChangeBalance::Set #:optional (UserValue (string->number (d-GetUserInput "Change Balance Value" "Please enter a balance value. Normally 1-128. 65 is center,  1 is leftmost emphasis and 128 is rightmost emphasis" "65"))))
(ChangeMidi::Parent "BalanceChange" "bal" "0xB$ 0x08" UserValue)
)

;;Number 10 - Pan. Where in the stereo field the channel sound will be placed.
(define* (ChangePan::Set #:optional (UserValue (string->number (d-GetUserInput "Change Pan Value" "Please enter a pan value. Normally 1-128. 65 is center,  1 is hard left and 128 is hard right" "65"))))
(ChangeMidi::Parent "PanChange" "pan" "0xB$ 0xA" UserValue)
)

;;Number 11 - Expression. Aka "Sub Volume" or "Percent Volume". The "real" volume. Use Volume as initial value for each staff/channel and change further cresc/desc with expression. 
(define* (ChangeExpression::Set #:optional (UserValue (string->number (d-GetUserInput "Change Expression Value" "Please enter an expression value. Normally 1-128. 65 is 50%,  1 is 0% and 128 is 100% of Volume." "128"))))
(ChangeMidi::Parent "ExpressionChange" "expr" "0xB$ 0xB" UserValue)
)

;;Number 64 - Hold Pedal On/Off (Right Piano Pedal)
(define* (ChangeHoldPedal::Set #:optional (UserValue (string->number (d-GetUserInput "Hold Pedal Value" "Please enter a hold pedal value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "HoldPedalChange" "hold" "0xB$ 0x40" UserValue)
)

;;Number 65 - Portamento On/Off
(define* (ChangePortamento::Set #:optional (UserValue (string->number (d-GetUserInput "Portamento Value" "Please enter a portamento value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "PortamentoChange" "port" "0xB$ 0x41" UserValue)
)
;;Number 5 - Portamento Time. Slides between 2 notes. Todo: 14-bit coarse/fine resolution. 0x000 to 0x3FFF where 0/1 the slowest rate. 

;;Number 66 - Sustenuto Pedal On/Off (Middle Grand Piano Pedal). All Notes active (without an note off yet) are taken.
(define* (ChangeHoldPedal::Set #:optional (UserValue (string->number (d-GetUserInput "Sustenuto Pedal Value" "Please enter a sustenuto pedal value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "SustenutoPedalChange" "hold" "0xB$ 0x42" UserValue)
)

;;Number 67 - Soft Pedal On/Off (Left Piano Pedal). Lowers the volume of any notes played.
(define* (ChangeSoftPedal::Set #:optional (UserValue (string->number (d-GetUserInput "Soft Pedal Value" "Please enter a soft pedal value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "SoftPedalChange" "hold" "0xB$ 0x43" UserValue)
)

;;Number 68 - Legato Pedal On/Off. Skips the attack portion of the VCA's envelope. For phrasing like wind or brass or guitar hammer-on.
(define* (ChangeLegatoPedal::Set #:optional (UserValue (string->number (d-GetUserInput "Legato Pedal Value" "Please enter a legato pedal value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "LegatoPedalChange" "hold" "0xB$ 0x44" UserValue)
)

;;Number 69 - Hold Pedal 2 On/Off. Longer release time but notes will fade out eventually.
(define* (ChangeHold2Pedal::Set #:optional (UserValue (string->number (d-GetUserInput "Hold2/Release Pedal Value" "Please enter a hold2/release pedal value. Normally 1-128. 1 to 64 is off, 65 to 128 is on" "1"))))
(ChangeMidi::Parent "Hold2PedalChange" "hold" "0xB$ 0x45" UserValue)
)
