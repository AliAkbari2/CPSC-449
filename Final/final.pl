%Question8
/*
    CPSC 449 Final Exam Prolog Part
    Refrence:
    Part B:
        From Assignment #5
        Ali Akbari
    Part A
*/
check_soc_dist([], []).
check_soc_dist([], _).
check_soc_dist([(A, B)|Tail], S) :- checkerIfMemberNotA(A, B, S), check_soc_dist(Tail, S).
check_soc_dist([(A, B)|Tail], S) :- checkerIfMemberNotANotB(A, B, S), check_soc_dist(Tail, S). 
check_soc_dist([(A, B)|Tail], S) :- checkerIfMemberNotB(A, B, S),check_soc_dist(Tail, S).

/*
    Helper function to check three conditions, if A is a member, and Not B
    If B is a member and Not A
*/
checkerIfMemberNotA(A, B, S) :- not(member(A,S)), member(B, S).
checkerIfMemberNotANotB(A, B, S) :- not(member(A,  S)), not( member(B, S)).
checkerIfMemberNotB(A, B, S) :- member(A,S), not(member(B, S)).

/*
    Part B
    Helper function to change list of tuple into straight list
*/
collect_verticesHelper([],[]).
collect_verticesHelper([(A, B)|Tail], [A, B|Return]) :- collect_vertices(Tail, Return).
/*
    collection of vertices from the given graph, uses the sort function 
*/
collect_vertices(G, Vs) :- collect_verticesHelper(G, Return), sort(Return, Vs). 

/*
    Part C
    Creates all possible social distance lists from given graph
*/
gen_soc_dist(G, S) :- gen_soc_dist_helper(G, S), check_soc_dist(G, S).
/*Helper function for gen_soc_dist that check if return value of gen_soc_dist is subset
*/
gen_soc_dist_helper(G, S) :- collect_vertices(G, A), takeSubset(A, S).
/*
    Helper function that takes a subset of a given list
*/
takeSubset([],[]).
takeSubset([H|T], [H|T1]):- takeSubset(T, T1).
takeSubset([_|T], T1):- takeSubset(T, T1).

