EllipseMorph subclass: #Ball
	instanceVariableNames: 'v'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bouncy'!
!Ball commentStamp: 'Jeff 8/29/2019 14:16' prior: 0!
A represent a single bouncy ball, a circle that moves with a velocity and bounce off the edges of its owner.!


!Ball methodsFor: 'positioning' stamp: 'Jeff 8/29/2019 14:18'!
randomizeVelocity: range
	"Given a Range, set the two-dimensional velocity such that its components are within that range"
	self setVelocity: range atRandom @ range atRandom.
! !

!Ball methodsFor: 'positioning' stamp: 'Jeff 8/29/2019 14:17'!
randomizePosition: aRect
		"Given a rectagle, set the position of this ball to somewhere inside it"
		self position: (aRect left to: aRect right) atRandom @ (aRect top to: aRect bottom) atRandom.
! !


!Ball methodsFor: 'initialization' stamp: 'JessicaHochrun 11/7/2019 21:36'!
advance
	"Update the position of the ball by velocity and check for wall collisions"
	
	((bounds left < owner bounds left) or: (bounds right > owner bounds right))
		ifTrue: [v := v*((-1)@1) ].
	((bounds top < owner bounds top) or: (bounds bottom > owner bounds bottom))
		ifTrue: [v := v*(1@(-1)) ].
	self position: (self position + v).
	"If the ball gets caught on the side while the window is changing size, 
	it goes back and forth repeatidly until the window is made bigger again."! !

!Ball methodsFor: 'initialization' stamp: 'Jeff 8/29/2019 13:53'!
initialize 
	"Create a new ball with default size and velocity"
	super initialize.
	self extent: 15@15.
	v:=5@5.
	self color: Color black.! !

!Ball methodsFor: 'initialization' stamp: 'Jeff 8/29/2019 13:53'!
setVelocity: aVelocity 
	"Set the velocity of the ball to the two-dimensional value aVelocity"

	v:=aVelocity.! !


Morph subclass: #Bouncy
	instanceVariableNames: 'balls active'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Bouncy'!
!Bouncy commentStamp: 'Jeff 8/29/2019 14:15' prior: 0!
I am a window that shows various balls bancing around.

You can click on me with the left mouse button to pause the action, and with the right mouse button to add another ball.!


!Bouncy methodsFor: 'drawing' stamp: 'Jeff 8/29/2019 14:18'!
drawOn: aCanvas
	"Draw the background of the window"

	super drawOn: aCanvas.
	aCanvas fillOval: (self bounds insetBy: 4) color: Color lightBlue.! !


!Bouncy methodsFor: 'event handling' stamp: 'Jeff 8/29/2019 14:08'!
handlesMouseDown:  anEvent
	^true.! !

!Bouncy methodsFor: 'event handling' stamp: 'JessicaHochrun 11/7/2019 21:31'!
mouseDown: anEvent
	"Called in response to a mouse click"
	| ball |
	anEvent redButtonPressed "left mouse button"
		ifTrue: [ active := active not.
			active
				ifTrue: [ self startStepping ]
				ifFalse: [ self stopStepping ]. ].
	anEvent yellowButtonPressed "right mouse button"
		ifFalse: [ ^ self ].
		ball := Ball new.
		ball randomizeVelocity: (-5 to: 5).
		ball position: anEvent cursorPoint.
		balls add: ball. 
		self addMorph: ball.
			"When the user clicks the right mouse button, we should add a new ball, at the location the 
			user clicked, with a random velocity. The ball should bounce around with the rest of them."! !


!Bouncy methodsFor: 'initialization' stamp: 'Jeff 8/29/2019 15:10'!
makeBalls: howMany
	"Creates an arbitrary number of balls and inserts them into the window"

	howMany timesRepeat: [ 
		|ball|
		ball := Ball new.
		ball randomizeVelocity: (-5 to: 5).
		ball randomizePosition: self bounds.
		balls add: ball.
		self addMorph: ball.
		 ].! !

!Bouncy methodsFor: 'initialization' stamp: 'Jeff 8/29/2019 15:09'!
initialize
	"initializes the local variables and sets up default window size"

	super initialize.
	active:=true.
	balls:=OrderedCollection new.
	self extent: 400@400.! !


!Bouncy methodsFor: 'stepping and presenter' stamp: 'Jeff 8/29/2019 14:19'!
stepTime
	"Indicates to Morphic the frequency with which the step method should be called"

	^50.! !

!Bouncy methodsFor: 'stepping and presenter' stamp: 'Jeff 8/29/2019 14:19'!
step
	"This method is called repeatedly every 50ms to advance the position of the balls"
	balls do: [ :ball | 
		ball advance.
		 ].! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Bouncy class
	instanceVariableNames: ''!

!Bouncy class methodsFor: 'navigating' stamp: 'Jeff 8/29/2019 13:55'!
go
	"Set up a bounch and show it on the desktop"

	|bouncy|
	bouncy := self new.
	bouncy makeBalls: 5.
	bouncy startStepping.
	bouncy openInWindow.
	^bouncy.! !
