/*
    CPSC 449 Fall 2020
    Assignment #5 Prolog 
    Ali Akbari
    30010402

*/

%Question1
/*
    Facts, database given in assignment for the predicates in question 1
*/
class(a).
class(b).
class(c).
interface(d).
interface(e).
interface(f).
extends(a, b).
extends(b, c).
implements(c, d).
extends(d, e).
extends(e, f).

/*
    Predicate to show if a given X is a subclass of a given Y
    Thus, if in the database X extend Y in a number of transitive closures
    Then return true else false
*/
subclass(X, Y) :- class(X), class(Y), extends(X, Y).
subclass(X, Y) :- class(X), class(Y), extends(X, Z), subclass(Z, Y).

/*
    Predicate to show if a given Y is an interface and a given X is the class,
    returns true if either X is an superclass or X implements an interface Z.
    Where Z is related to Y via transitive closure of extends.
    Else return false.
*/
superinterface(Y, X) :- interface(Y), class(X), implements(X, Y).
superinterface(Y, X) :- interface(Y), class(X), extends(X, Z), superinterface(Y, Z).
superinterface(Y, X) :- interface(Y), class(X), extends(Z, Y), superinterface(Z, X).

%Question2
/*
    Helper function to change a tupple list to straight list.

*/
tupleTolist([], []).
tupleTolist([(A, B)|Tail], [A, B|Return]) :- tupleTolist(Tail, Return).

/*
    Helper function to make the list from the tupleToList into the straightlist
    Calls another function to sort helper function
*/
straightList(L, R1) :- tupleTolist(L, R), removeDuplicatesVertices(R, R1).

/*
    Helper function to then remove the duplicated vertices by using the sorting.
    Returns a straight list.
*/
removeDuplicatesVertices(L1, L2) :- sort(L1,L2).

/*
    Helper function that uses built-in permutations, to generate permutation of the of the straight list
    and the graph list while sorting and finding 
*/
permList(R, L) :- straightList(R, L1), listPerm(L1, R2), graphPerm(R,L2), uniMemberGraph(L2,R2,L3), removeDuplicatesCover(L3,L).
/*
    Helper function to then remove the duplicated covers by using the sorting.
    Returns a straight list.
*/
removeDuplicatesCover(L,L2) :- sort(L,L2).
/*
    One list permutation made for graph list passed in
*/
graphPerm(R, L2) :- permutation(R, L2).
/*
    One list permutation made for list made
*/
listPerm(L1, L2) :- permutation(L1, L2).

/*
    Helper function makes a list of member from both permuatation list (permList and graphperm)
    Called by permList helper function.
    Makes a unique list based on the member function, i.e using the == to check for intersections.
*/
uniMemberGraph([], _, []).
uniMemberGraph([(A,B)|Tail], [Head|Tail1], [Head|Return]) :- Head == A, uniMemberGraph(Tail, [Head|Tail1],  Return).
uniMemberGraph([(A,B)|Tail], [Head|Tail1], [Head|Return]) :- Head == B, uniMemberGraph(Tail, [Head|Tail1],  Return).
uniMemberGraph(G1, [Head|Tail1],  Return) :-  uniMemberGraph(G1, Tail1, Return).

/*
    Main function, calls helper function permList
    Takes in a graph, i.e tuple list
    Returns an iteration of Cover
*/
vertex_cover(Graph, Cover) :- permList(Graph, Cover).



