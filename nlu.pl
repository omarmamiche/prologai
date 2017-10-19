:- discontiguous(block/1).
:- discontiguous(color/2).
:- discontiguous(size/2).
:- discontiguous(shape/2).
:- discontiguous(locatedOn/2).

/*
 * nlu.pl : natural language processing
 *
 *
 * Authors:
 *		Daniel Moscuzza 
 *		Omar Mamiche 
 */




/******************* PARSER **********************/

what(Words, Block) :- np(Words, Block).

/* Noun phrase can be a proper name or can start with an article */

the(Rest,What) :- np2(Rest, What1), not What = What1.

% Handle the word "the"
np([the|Rest], What) :- np2(Rest, What), not the(Rest,What).

%np([Name],Name) :- proper_noun(Name).
np([Art|Rest], What) :- article(Art), np2(Rest, What).

/* If a noun phrase starts with an article, then it must be followed
   by another noun phrase that starts either with an adjective
   or with a common noun. */

np2([Adj|Rest],What) :- adjective(Adj,What), np2(Rest, What).
np2([Noun|Rest], What) :- common_noun(Noun, What), mods(Rest,What).
np2([Noun|Rest], What) :- mods(Rest,What).

/* Modifier(s) provide an additional specific info about nouns.
   Modifier can be a prepositional phrase followed by none, one or more
   additional modifiers.  */

mods([], _).
mods(Words, What) :-
	appendLists(Start, End, Words),
	prepPhrase(Start, What), mods(End, What).

prepPhrase([Prep|Rest], What) :-
	preposition(Prep, What, Block), np(Rest, Block).

appendLists([], L, L).
appendLists([H|L1], L2, [H|L3]) :-  appendLists(L1, L2, L3).

/****************** END PARSER *******************/

% Database
block(block01). color(block01,green).  size(block01,small).  shape(block01,cube).    locatedOn(block01,1).	
block(block02). color(block02,blue).   size(block02,medium). shape(block02,pyramid). locatedOn(block02,block01).
block(block03). color(block03,red).    size(block03,big).    shape(block03,wedge).   locatedOn(block03,2).
block(block04). color(block04,yellow). size(block04,huge).   shape(block04,cube).    locatedOn(block04,3).
block(block05). color(block05,pink).   size(block05,small).  shape(block05,pyramid). locatedOn(block05,block04).

block(block06). color(block06,orange). size(block06,small).  shape(block06,wedge).   locatedOn(block06,4).
block(block07). color(block07,green).  size(block07,medium). shape(block07,pyramid). locatedOn(block07,5).
block(block08). color(block08,blue).   size(block08,big).    shape(block08,wedge).   locatedOn(block08,block09).
block(block09). color(block09,red).    size(block09,huge).   shape(block09,cube).    locatedOn(block09,6).
block(block10). color(block10,yellow). size(block10,small).  shape(block10,pyramid). locatedOn(block10,7).

block(block11). color(block11,pink).   size(block11,small).  shape(block11,cube).    locatedOn(block11,8).
block(block12). color(block12,orange). size(block12,medium). shape(block12,pyramid). locatedOn(block12,block14).
block(block13). color(block13,green).  size(block13,big).    shape(block13,wedge).   locatedOn(block13,9).
block(block14). color(block14,blue).   size(block14,huge).   shape(block14,cube).    locatedOn(block14,block11).
block(block15). color(block15,blue).   size(block15,huge).   shape(block15,wedge).   locatedOn(block15,10).

/*	These atomic statements mark areas that are 
	beside eachother, the first area being to 
	left of the other
*/
justLeftOf(10,9). justLeftOf(9,8). justLeftOf(8,7). justLeftOf(7,6). justLeftOf(6,5). justLeftOf(5,4). justLeftOf(4,3). justLeftOf(3,2). justLeftOf(2,1). 

/*	This rule is checking whether two blocks
	are beside eachother
*/
beside(X,Y) :- block(X), block(Y), locatedOn(X,XArea), locatedOn(Y,YArea), justLeftOf(XArea,YArea).
beside(X,Y) :- block(X), block(Y), locatedOn(X,XArea), locatedOn(Y,YArea), justLeftOf(YArea,XArea).

/*	This predicate checks whether a block is
	on top of another block and ensures that
	the shape beneath any other shape is a
	cube.
*/
above(X,Y) :- block(X), block(Y), shape(Y,cube), locatedOn(X,Y).
above(X,Y) :- block(X), block(Y), block(Z), locatedOn(X,Z), above(Z,Y).

/*	This predicate checks whether a block is
	underneath another block and ensures that
	the shape beneath any other shape is a
	cube.
*/
below(X,Y) :- block(X), block(Y), shape(X,cube), locatedOn(Y,X).
below(X,Y) :- block(X), block(Y), block(Z), locatedOn(Y,Z), below(Z,X).

/*	This rule is used to determine if a block X
	is located somewhere to the left of block Y
*/
leftOf(X,Y) :- locatedOn(X,XArea), locatedOn(Y,YArea), not block(XArea), not block(YArea), XArea < YArea.
leftOf(X,Y) :- locatedOn(X,XArea), locatedOn(Y,YArea), block(XArea), not block(YArea), leftOf(XArea,Y).
leftOf(X,Y) :- locatedOn(X,XArea), locatedOn(Y,YArea), not block(XArea), block(YArea), leftOf(X,YArea).
leftOf(X,Y) :- locatedOn(X,XArea), locatedOn(Y,YArea), block(XArea), block(YArea), leftOf(XArea,YArea).

/*	This rule is used to determine if a block Y
	is located somewhere to the left of block X
*/
rightOf(X,Y) :- leftOf(Y,X).

% LEXICON
article(a).
article(any).
article(and).
article(of).

common_noun(block,X) :- block(X).
common_noun(table,X) :- not block(X).

adjective(small,X) :- size(X,small).
adjective(medium,X) :- size(X,medium).
adjective(big,X) :- size(X,big).
adjective(huge,X) :- size(X,huge).
adjective(massive,X) :- size(X,massive).

adjective(cube,X) :- shape(X,cube).
adjective(wedge,X) :- shape(X,wedge).
adjective(pyramid,X) :- shape(X,pyramid).

adjective(black,X) :- color(X,black).
adjective(green,X) :- color(X,green).
adjective(blue,X) :- color(X,blue).
adjective(red,X) :- color(X,red).
adjective(yellow,X) :- color(X,yellow).
adjective(pink,X) :- color(X,pink).
adjective(orange,X) :- color(X,orange).

preposition(right,X,Y) :- rightOf(X,Y).
preposition(left,X,Y) :- leftOf(X,Y).
preposition(on,X,Y) :- locatedOn(X,Y).
preposition(above,X,Y) :- above(X,Y).
preposition(below,X,Y) :- below(X,Y).
preposition(beside,X,Y) :- beside(X,Y).

/*	
	With regards to the between preposition below, what we were trying to do was use the word
	"and" (z conjunction) in a similar fashion as that of the preposition "between". We understand
	that any block following the conjunction "and" is the block which is to the right of the
	block in question (X). In order for the "and" conjunction to function correctly, we think
	that the parser above will need to be modified to handle conjunctions.
*/

preposition(between,X,Y) :- rightOf(Y,X).
%conjunction(and,X,Y) :- leftOf(Y,X).

