'From Pharo7.0.4 of 2 November 2019 [Build information: Pharo-7.0.4+build.172.sha.ecc855d08d3512e1bc965eec6290561141d1b35a (64 Bit)] on 7 November 2019 at 10:28:55.925825 pm'!

!BlockClosure methodsFor: 'controlling' stamp: 'JessicaHochrun 11/7/2019 22:26'!
repeatUntil: expr
	"repeats body until given expression evaluates to true - body always executes at least once"

	self value. 
	expr value
		ifFalse: [ self repeatUntil: expr ].		
	"Example has 0 2 4 6 as output, but this goes to 8 since > is used instead of >=
	stops at 6 if x >= 6 is used"
	"Similar for 7 - runs once, so x is 9, then stops"! !
