m(X,Y,S) :- setof(Z, (member(Z, X), member(Z, Y)), S).
m( _, _, []).