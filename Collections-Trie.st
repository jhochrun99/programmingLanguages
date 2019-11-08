Collection subclass: #Trie
	instanceVariableNames: 'char nexts terminal'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Trie'!
!Trie commentStamp: 'Jeff 8/29/2019 00:40' prior: 0!
I store a set of unordered strings in a trie data structure.

Implementation notes:
I have three instance variables:
	char - a single Character stored at this node in the Trie
	nexts - an OrderedCollection of child nodes
	terminal - a Boolean indicated if this node is a terminal node
	!


!Trie methodsFor: 'removing' stamp: 'Jeff 8/28/2019 19:15'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Do nothing, we don't remove items from the trie."

	self notYetImplemented ! !


!Trie methodsFor: 'instance creation' stamp: 'Jeff 8/29/2019 00:38'!
lookup: str
	"Determine if the node and its children contains the given string, returns true or false"

	"If this is a terminal node and we're looking for an empty string, answer true"
	terminal & str isEmpty
		ifTrue: [ ^ true ].
		
	"If we're not a terminal node, but we're looking for an empty string, return false"
	str isEmpty
		ifTrue: [ ^ false ].
		
	"Otherwise, look through child nodes to see if we can find one that matches"
	^ nexts
		anySatisfy: [ :next | next char = str first & (next lookup: str allButFirst) ]! !


!Trie methodsFor: 'initialization' stamp: 'Jeff 8/28/2019 19:14'!
setChar: c
	"Set the value of the character represented in this node"

	char:=c.! !

!Trie methodsFor: 'initialization' stamp: 'Jeff 8/28/2019 21:32'!
initialize
	"Initialize an empty instance"

	nexts := OrderedCollection new.
	terminal := false! !


!Trie methodsFor: 'accessing' stamp: 'Jeff 8/29/2019 00:25'!
add: str
	"Insert a string into the trie at this point"

	"If we're inserting an empty string, then this must be a
	terminal node, and we're done"
	str isEmpty
		ifTrue: [ terminal := true.
			^ self ].
		
	"Otherwise, look through all child nodes until
	we find one that matches the first character
	in the string, and recurse."
	nexts
		do: [ :next | 
			next char = str first
				ifTrue: [ next add: str allButFirst.
					^ self ] ].
			
	"If we can't find a matching child node,
	add one, then recurse."
	nexts
		add:
			(self class new
				setChar: str first;
				add: str allButFirst).
	^ self! !

!Trie methodsFor: 'accessing' stamp: 'Jeff 8/28/2019 21:07'!
char
	"answer the character represented by this node"
	^char.! !


!Trie methodsFor: 'helper methods' stamp: 'JessicaHochrun 11/7/2019 22:00'!
doAction: str withBlock: aBlock
	"recursive helper for do: method"

	| strUpdate |
	strUpdate := char isNil
		ifTrue: [ str ]
		ifFalse: [ str asString, char asString ].
	terminal
		ifTrue: [ aBlock value: strUpdate ].
	nexts do: [ :next | next doAction: strUpdate withBlock: aBlock ]! !


!Trie methodsFor: 'enumerating' stamp: 'JessicaHochrun 11/7/2019 21:56'!
do: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self doAction: '' withBlock: aBlock.! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Trie class
	instanceVariableNames: ''!

!Trie class methodsFor: 'tests' stamp: 'Jeff 8/28/2019 22:43'!
test
	"Test the Trie class"

	| tn |
	"Initialize the trie and add a bunch of stuff."
	tn := Trie new.
	self assert: tn sorted = #().
	tn addAll: {'noodle' . 'new' . 'noon' . 'spatula' . 'spat'}.
	self assert: tn sorted = #('new' 'noodle' 'noon' 'spat' 'spatula').
	tn add: 'new'.	"Adding a string twice should do nothing."
	tn add: 'another'.
	self assert: tn sorted = #('another' 'new' 'noodle' 'noon' 'spat' 'spatula').
	self assert: (tn lookup: 'noodle').
	self assert: ((tn lookup: 'not') not). 
	self assert: ((tn lookup: 'ne') not). 
	self assert: ((tn lookup: 'spatu') not). 
	self assert: (tn lookup: 'spatula'). 
	Transcript
		clear;
		show: tn sorted! !
