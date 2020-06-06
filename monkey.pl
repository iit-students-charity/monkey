/** <module> Monkey and cage problem solver
@author Kliujeu Aliaksandr
@license Apache 2.0
*/

nth01(I, A, E):-
    nth0(I, A, E),
    !.

spec_el([]).
spec_el(nl).

elem([my]).
elem([sk]).
elem([bx]).
elem([ba]).
elem([my, sk]).
elem([my, bx]).
elem([my, ba]).
elem([sk, bx]).
elem([sk, ba]).
elem([bx, ba]).
elem([my, sk, bx]).
elem([my, sk, ba]).
elem([my, bx, ba]).
elem([sk, bx, ba]).
elem([my, sk, bx, ba]).

valid_el(E) :-
    (   elem(E)
    ;   spec_el(E)
    ).

solution(S) :-
    member([my, sk, bx, ba], S).

%! check_elem(+List:list) is det.
%
%  Calculates Index of the Element in List.
check_elem([]).
check_elem([E|T]) :-
    valid_el(E),
    check_elem(T).

%! valid(+Arg) is nondet.
%
%  True if the field is valid.
valid(A) :-
    check_elem(A),
    length(A, L),
    I is L - 1,
    nth01(I, A, nl),
    flatten(A, FA),
    delete(FA, nl, DFA),
    is_set(DFA),
    member(my, FA),
    member(sk, FA),
    member(bx, FA),
    member(ba, FA).
/** <tests>
?- valid([[my, sk, bx, ba]]),
   valid([[my, sk], [], [bx], [ba], nl]),
   not(valid([[my, sk], [sk]])),
   not(valid([[my, sk], [bx], [ba], [sk]])),
   not(valid([])),
   not(valid([[my], [my, sk]])),
   not(valid([[my, sk], 3, [bx], [ba], nl])),
   not(valid([[my], [my], [sk, bx, ba]])),
   not(valid([[sk], [my, bx], [my, bx, ba], nl])),
   not(valid([[sk], [my, bx], [my, ba], nl])).
*/

move(A, B) :-
    length(A, L),
    length(B, L),
    valid(A),
    valid(B),
    % Interaction rules
    (   % Not throwing items rule
        (   member(C, A), C = [my|_], nth01(I, A, C),
            nth01(I, B, D), D \= [my|_],
            member(M, D), \+ member(M, C)
        ->  false
        ;   true
        ),
        % Not moving without monkey rule
        (   member(C, A), C \= [my|_], nth01(I, A, C),
            nth01(I, B, D), D \= [my|_],
            C \= D
        ->  false
        ;   true
        ),
        % Not moving banana rule
        (   member(C, A), member(ba, C),
            nth01(I, A, C), nth01(I, B, D),
            \+ member(ba, D)
        ->  false
        ;   true
        ),
        % Moving box and stick separately rule
        (   SB = [[my, sk, bx], [my, sk, bx, ba]],
            member(C, SB), member(D, SB),
            nth01(I1, A, C), nth01(I2, B, D), I1 \= I2
        ->  false
        ;   true
        ),
        % Grabbing stick rule
        (   nth01(I1, A, [sk|_]), nth01(I2, B, [my, sk|_])
        ->  I1 = I2
        ;   true
        ),
        % Grabbing box rule
        (   nth01(I1, A, [bx|_]), nth01(I2, B, [my, bx|_])
        ->  I1 = I2
        ;   true
        ),
        % Grabbing banana rule
        (   solution(B)
        ->  (   subset([[my, sk], [bx, ba]], A)
                ;   subset([[my, bx], [sk, ba]], A)
            )
        ;   true
        )
    ),
    % Moving rules
    (   nth01(TSZ, A, nl), nth01(TSZ, B, nl),
        M1 = [my|_], M2 = [my|_],
        nth01(IA, A, M1), nth01(IB, B, M2),
        not(nth01(IB, A, nl)), not(nth01(IA, B, nl)),
        S1 is IA + 1, SM1 is IA - 1,
        S3 is IA + TSZ, SM3 is IA - TSZ,
        S4 is S3 + 1, SM4 is SM3 - 1,
        S5 is S4 + 1, SM5 is SM4 - 1,
        (   nextto(nl, M1, A)
        ->  (   nextto(M1, nl, A)
            ->  false
            ;   memberchk(IB, [S1, S4, SM4, S5, SM5])
            )
        ;   (   nextto(M1, nl, A)
            ->  memberchk(IB, [SM1, S4, SM4, S5, SM5])
            ;   memberchk(IB, [S1, SM1, S3, SM3, S4, SM4, S5, SM5])
            )
        )
    ).
/** <tests>
?- move([[my], [ba], [sk, bx], nl], Ы)
?- move([
        [  ], [my], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ], [
        [  ], [], [my], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ]),
   not(move([
        [  ], [my], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ], [
        [  ], [], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [my], [bx], nl
        ])),
    move([
        [  ], [my], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ], [
        [  ], [], [], nl,
        [sk], [ba], [my], nl,
        [  ], [  ], [bx], nl
        ])
?- move([
        [  ], [my], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ], Ў).
*/

print_position(nl) :-
    nl.
print_position(S) :-
    (   S=[my, sk, bx, ba],
        write('|🐒🧹📦🍌|')
    ;   S=[sk, bx, ba],
        write('|⬛🧹📦🍌|')
    ;   S=[my, bx, ba],
        write('|🐒⬛📦🍌|')
    ;   S=[my, sk, ba],
        write('|🐒🧹⬛🍌|')
    ;   S=[my, sk, bx],
        write('|🐒🧹📦⬛|')
    ;   S=[my, sk],
        write('|🐒🧹⬛⬛|')
    ;   S=[my, bx],
        write('|🐒⬛📦⬛|')
    ;   S=[my, ba],
        write('|🐒⬛⬛🍌|')
    ;   S=[sk, bx],
        write('|⬛🧹📦⬛|')
    ;   S=[sk, ba],
        write('|⬛🧹⬛🍌|')
    ;   S=[bx, ba],
        write('|⬛⬛📦🍌|')
    ;   S=[my],
        write('|🐒⬛⬛⬛|')
    ;   S=[sk],
        write('|⬛🧹⬛⬛|')
    ;   S=[bx],
        write('|⬛⬛📦⬛|')
    ;   S=[ba],
        write('|⬛⬛⬛🍌|')
    ;   S=[],
        write('|⬛⬛⬛⬛|')
    ).

%! print_field(+Arg:list) is det.
%
%  Prints the field's table.
print_field([]).
print_field([A|Tail]) :-
    print_position(A),
    print_field(Tail).

%! print_way(+Arg:list) is det.
%
%  Prints the way to solve the problem.
print_way([]).
print_way([A|Tail]) :-
    print_field(A),
    nl,
    print_way(Tail).

%! check_path(+S1, +S2, +Way) is det.
%
%  True if the movement is possible and the second state
%  is not present in the way(to avoid recursion).
check_path(A, B, Way) :-
    move(A, B),
    not(member(B, [A|Way])).

find_way([[Result|Way]|_], Result) :-
    reverse([Result|Way], RWay),
    write('Solved!'),
    nl,
    print_way(RWay).
find_way([[Temp|Way]|B], Result) :-
    (   setof([Next, Temp|Way],
              check_path(Temp, Next, Way),
              A),
        append(B, A, Way1),
        !,
        find_way(Way1, Result)
    ;   find_way(B, Result)
    ).

run(Start, Result) :-
    find_way([[Start]], Result),
    !.

/** <examples>
?- run([
		[my], [ba], [sk, bx], nl
        ], [
        [], [my, sk, bx, ba], [], nl
        ]).
% Following examples currently doesn't work in SWISH, because of
% time limitations:
?- run([
		[my], [ba], nl,
		[sk], [bx], nl
        ], [
        [], [my, sk, bx, ba], nl,
        [], [              ], nl
        ]).
?- run([
		[my], [ba], [  ], nl,
        [sk], [  ], [bx], nl
        ],[
        [], [my, sk, bx, ba], [], nl,
        [], [              ], [], nl
        ]).
?- run([
        [  ], [my], [  ], nl,
        [sk], [ba], [  ], nl,
        [  ], [  ], [bx], nl
        ], [
        [], [              ], [], nl,
        [], [my, sk, bx, ba], [], nl,
        [], [              ], [], nl
        ]).
*/
