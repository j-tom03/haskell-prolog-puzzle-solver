generator3( X ) :-
    between(1000, 1000000, X ),
    perfect_square( X ).

perfect_square( X ) :-
    sqrt( X, R ),
    floor(R)^2 =:= X.

tester3( X ) :-
    digits(X, XS),
    different( XS ),
    length( XS, L ),
    D is L - 2,
    drop_list( D, XS, [X3, X4] ),
    take_list( 3, XS, [F1, F2, F3] ),
    X4 =:= L,
    contains_one_zero( XS ),
    odd( X3 ),
    X3 =\= 0,
    F2 =\= 0,
    F3 =\= 0,
    0 is mod(X3, F1),
    0 is mod(F2, F1),
    0 is mod(F3, F1).
    
digits( N, [N] ) :-
    N < 10.
digits( N, W ) :-
    N >= 10,
    div_mod(N, 10, D, M),
    digits( D, R ),
    append( R, [M], W ).

div_mod( A, B, D, M ) :-
    D is A div B,
    M is A mod B.

different([]).
different([_]).
different([H|T]) :-
    \+ member(H, T),
    different( T ).

contains_one_zero( [0|T] ) :-
    \+ contains_zero( T ).
contains_one_zero( [_|T] ) :-
    contains_one_zero( T ).

contains_zero( [0|_] ).
contains_zero( [_|T] ) :-
    contains_zero( T ).

odd( X ) :-
    1 is mod(X, 2).

drop_list( _ , [] , [] ).
drop_list( 0 , X , X ).
drop_list( N , [ _ | T ] , W ) :-
	N1 is N - 1,
	drop_list( N1 , T , W ).
        
take_list( _ , [] , [] ).
take_list( 0 , _ , [] ).
take_list( N , [ H | T ] , [ H | W ] ) :-
	N1 is N - 1,
	take_list( N1 , T , W ).

x_generator3( N ) :-
	x_generator3_loop(
		[ 1024 , 9409 , 23716 , 51529
		, 123904 , 185761 , 868624 , 962361
		, 982081 , 1000000 ] , 0 , N ).
x_generator3_loop( [] , C , C ).
x_generator3_loop( [T|TS] , C , N ) :-
	generator3( T ),
	C1 is C + 1,
	x_generator3_loop( TS , C1 , N ).
x_generator3_loop( [_|TS] , C , N ) :-
	x_generator3_loop( TS , C , N ).

x_tester3( N ) :-
	x_tester3_loop(
		[ 123056 , 128036 , 139076 , 142076
		, 148056 , 159076 , 173096 , 189036
		, 193056 , 198076 ] , 0 , N ).
x_tester3_loop( [] , C , C ).
x_tester3_loop( [T|TS] , C , N ) :-
	tester3( T ),
	C1 is C + 1,
	x_tester3_loop( TS , C1 , N ).
x_tester3_loop( [_|TS] , C , N ) :-
	x_tester3_loop( TS , C , N ).

main :-
    generator3(X), tester3( X ), write(X).