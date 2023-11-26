% vim: syntax=prolog et ts=4 sw=4 ai

my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).

:- begin_tests(p1_1).
    test(none, fail) :- my_last(_, []).
    test(one) :- my_last(1, [1]).
    test(two) :- my_last(2, [1, 2]).
    test(three) :- my_last(3, [1, 2, 3]).
:- end_tests(p1_1).

second_last(X, [X,_]).
second_last(X, [_|T]) :- second_last(X, T).

:- begin_tests(p1_2).
    test(none, fail) :- second_last(_, []).
    test(one, fail) :- second_last(_, [1]).
    test(two) :- second_last(1, [1, 2]).
    test(three) :- second_last(2, [1, 2, 3]).
:- end_tests(p1_2).

element_at(_, _, I) :- I < 1, !, fail.
element_at(_, [], _) :- !, fail.
element_at(_, L, I) :- length(L, LL), I > LL, !, fail.
element_at(X, [X|_], 1).
element_at(X, [_|T], I) :- J is I - 1, element_at(X, T, J).

elements_in(0,[]).
elements_in(X,[_|T]) :- elements_in(N,T), X is N + 1.

rev([], []) :- !.
rev([H|T],L) :- rev(T,T2), append([T2, [H]], L), !.
rev(L,[H|T]) :- rev(T,T2), append([T2, [H]], L).

palindrome(L) :- reverse(L, L).

my_flatten([], []).
my_flatten([H|T], L) :- is_list(H), my_flatten(H, H2), my_flatten(T, T2), append(H2, T2, L).
my_flatten([H|T], [H|T2]) :-  not(is_list(H)), my_flatten(T, T2).

compress([], []).
compress([X], [X]).
compress([H,H|T], L) :- compress([H|T], L).
compress([X,Y|T], [X|L]) :- X \= Y, compress([Y|T], L).

% 1.09 (**) Pack consecutive duplicates of list elements into sublists. 
%   If a list contains repeated elements they should be placed in separate sublists.
% Example:
%   ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
wrap([],[]).
wrap([X|Xs],[[X]|Ys]) :- wrap(Xs,Ys).
condense([Xs],[Xs]) :- is_list(Xs).
condense([[X|Xs],[X]|Ys], Zs) :- condense([[X,X|Xs]|Ys], Zs).
condense([[X|Xs],[Y]|Ys], [[X|Xs]|Zs]) :- X \= Y, condense([[Y]|Ys],Zs).
pack([],[]).
pack(Xs,L) :- wrap(Xs,Ys), condense(Ys,L).

% 1.10 (*) Run-length encoding of a list.
%   Use the result of problem 1.09 to implement the so-called run-length 
%   encoding data compression method. Consecutive duplicates of elements are 
%   encoded as terms [N,E] where N is the number of duplicates of the element 
%   E.
% Example:
%   ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],[1,b],[2,c],[2,a],[1,d],[4,e]]
encode_item([X|Xs],[Nx,X]) :- length([X|Xs], Nx).
encode_packed([],[]).
encode_packed([X|Xs], [Y|Ys]) :- encode_item(X,Y), encode_packed(Xs,Ys).
encode([],[]).
encode([X|Xs],Zs) :- pack([X|Xs], Ys), encode_packed(Ys,Zs).

% 1.11 (*) Modified run-length encoding.
%   Modify the result of problem 1.10 in such a way that if an element has no
%   duplicates it is simply copied into the result list. Only elements with 
%   duplicates are transferred as [N,E] terms.
% Example:
%   ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]]
simplify([],[]).
simplify([[1,X]|Xs], [X|Ys]) :- simplify(Xs,Ys).
simplify([[N,X]|Xs], [[N,X]|Ys]) :- N > 1, simplify(Xs,Ys).
encode_modified([], []).
encode_modified([X|Xs], Zs) :- encode([X|Xs], Ys), simplify(Ys,Zs).

% 1.12 (**) Decode a run-length encoded list.
%   Given a run-length code list generated as specified in problem 1.11. 
%   Construct its uncompressed version.
decode([], []).
decode([[1,X]|Xs], [X|Ys]) :- decode(Xs, Ys).
decode([[N,X]|Xs], [X|Ys]) :- N > 1, N2 is N - 1, decode([[N2,X]|Xs], Ys).
decode([X|Xs], [X|Ys]) :- \+ is_list(X), decode(Xs, Ys).

% 1.13 (**) Run-length encoding of a list (direct solution).
%   Implement the so-called run-length encoding data compression method 
%   directly. I.e. don't explicitly create the sublists containing the 
%   duplicates, as in problem 1.09, but only count them. As in problem 1.11, 
%   simplify the result list by replacing the singleton terms [1,X] by X.
% Example:
%   ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([],[]).
encode_direct([X],[X]) :- \+ is_list(X).
encode_direct([X,X|Xs],L) :- \+ is_list(X), encode_direct([[2,X]|Xs],L).
encode_direct([X,Y|Xs],[X|Zs]) :- X \= Y, \+ is_list(X), \+ is_list(Y), encode_direct([Y|Xs],Zs).
encode_direct([[N,X]],[[N,X]]) :- N > 0.
encode_direct([[N,X],X|Xs],L) :- N2 is N + 1, encode_direct([[N2,X]|Xs],L).
encode_direct([[N,X],Y|Xs],[[N,X]|L]) :- X \= Y, encode_direct([Y|Xs],L).

% 1.14 (*) Duplicate the elements of a list.
% Example:
%     ?- dupli([a,b,c,c,d],X).
%     X = [a,a,b,b,c,c,c,c,d,d]
dupli([],[]).
dupli([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).

% 1.15 (**) Duplicate the elements of a list a given number of times.
% Example:
%     ?- dupli([a,b,c],3,X).
%     X = [a,a,a,b,b,b,c,c,c]
dupli([X],1,[X]).
dupli([X],N,[X|Xs]) :- N > 1, N2 is N - 1, dupli([X],N2,Xs).
dupli([X,Y|Ys], N, L) :- dupli([X], N, Xs), dupli([Y|Ys], N, Zs), append(Xs, Zs, L).

% 1.16 (**) Drop every N'th element from a list.
% Example:
%     ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%     X = [a,b,d,e,g,h,k]
drop([],_,[]).
drop([_|_],1,[]).
drop([X],N,[X]) :- N > 1.
drop([X1,X2|Xs], N, L2) :- N > 1, drop([X1,X2|Xs], N, L2, N).
drop([_|Xs],1,Ys,N) :- drop(Xs,N,Ys).
drop([X|Xs],Nx,[X|Ys],N) :- Nx > 1, Ny is Nx - 1, drop(Xs,Ny,Ys,N).

% 1.17 (*) Split a list into two parts; the length of the first part is given.
%     Do not use any predefined predicates.
% Example:
%     ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%     L1 = [a,b,c]
%     L2 = [d,e,f,g,h,i,k]
split([X|Xs],1,[X],Xs).
split([X|Xs],N,[X|Ys],Zs) :- N > 1, N2 is N - 1, split(Xs, N2, Ys, Zs).

% 1.18 (**) Extract a slice from a list.
%   Given two indices, I and K, the slice is the list containing the elements
%   between the I'th and K'th element of the original list (both limits 
%   included). Start counting the elements with 1.
% Example:
%   ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%   L = [c,d,e,f,g]
slice(Xs,1,K,Ys) :- split(Xs,K,Ys,_).
slice([_|Xs],I,K,Ys) :- I > 1, I2 is I - 1, K2 is K - 1, slice(Xs,I2,K2,Ys).

% 1.19 (**) Rotate a list N places to the left.
% Examples:
%     ?- rotate([a,b,c,d,e,f,g,h],3,X).
%     X = [d,e,f,g,h,a,b,c]
%
%     ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%     X = [g,h,a,b,c,d,e,f]
%
%     Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem 1.17.
rotate(L,0,L).
rotate(L1,N,L2) :- N < 0, length(L1, Length), N2 is Length - (-N mod Length), rotate(L1,N2,L2).
rotate(L1,N,L2) :- N > 0, length(L1, Length), N >= Length, N2 is N mod Length, rotate(L1,N2,L2).
rotate(L1,N,L4) :- N > 0, length(L1, Length), N < Length, split(L1,N,L2,L3), append(L3,L2,L4).

% 1.20 (*) Remove the K'th element from a list.
% Example:
%     ?- remove_at(X,[a,b,c,d],2,R).
%     X = b
%     R = [a,c,d]
remove_at(X,[X|R],1,R).
remove_at(X,[Y|Ys],N,R) :- N > 1, N2 is N - 1, remove_at(X,Ys,N2,R2), append([Y],R2,R).

% 1.21 (*) Insert an element at a given position into a list.
% Example:
%     ?- insert_at(alfa,[a,b,c,d],2,L).
%     L = [a,alfa,b,c,d]
insert_at(X,[],1,[X]).
insert_at(X,[Y|Ys],1,[X,Y|Ys]).
insert_at(X,[Y|Ys],N,[Y|Zs]) :- N > 1, N2 is N - 1, insert_at(X,Ys,N2,Zs).

% 1.22 (*) Create a list containing all integers within a given range.
% Example:
%     ?- range(4,9,L).
%     L = [4,5,6,7,8,9]
range(X,X,[X]).
range(From,To,[From|Xs]) :- From < To, From2 is From+1, range(From2,To,Xs).
range(From,To,[From|Xs]) :- From > To, From2 is From-1, range(From2,To,Xs).

% 1.23 (**) Extract a given number of randomly selected elements from a list.
%     The selected items shall be put into a result list.
% Example:
%     ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
%     L = [e,d,a]
%
%     Hint: Use the built-in random number generator random/2 and the result of problem 1.20.
rnd_select(_,0,[]).
rnd_select([X],1,[X]).
rnd_select(L,N,[Y|Zs]) :- 
    N >= 1,
    length(L,Length), 
    Length > 1,
    N =< Length,
    % There is no built-in random/2 and random/3 excludes the upper bound.
    random_between(1,Length,Random), 
    remove_at(Y,L,Random,Ys),
    N2 is N - 1,
    rnd_select(Ys,N2,Zs).

% 1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
%     The selected numbers shall be put into a result list.
% Example:
%     ?- lotto(6,49,L).
%     L = [23,1,17,33,21,37]
%
%     Hint: Combine the solutions of problems 1.22 and 1.23.
lotto(N,U,L) :- range(1,U,R), rnd_select(R,N,L).

% 1.25 (*) Generate a random permutation of the elements of a list.
% Example:
%     ?- rnd_permu([a,b,c,d,e,f],L).
%     L = [b,a,d,c,e,f]
%
%     Hint: Use the solution of problem 1.23.
rnd_permu(L1,L2) :- length(L1,N), rnd_select(L1,N,L2).

% 1.26 (**) Generate the combinations of K distinct objects chosen from the N 
%   elements of a list.
%   In how many ways can a committee of 3 be chosen from a group of 12 people? 
%   We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the 
%   well-known binomial coefficients). For pure mathematicians, this result may
%   be great. But we want to really generate all the possibilities (via 
%   backtracking).
% Example:
%   ?- combination(3,[a,b,c,d,e,f],L).
%   L = [a,b,c] ;
%   L = [a,b,d] ;
%   L = [a,b,e] ;
%   ...
combination(1, [X|_], [X]).
combination(1, [_|Xs], Ys) :- combination(1,Xs,Ys).
combination(N, [X|Xs], [X|Ys]) :- N > 1, N2 is N-1, combination(N2,Xs,Ys).
combination(N, [_|Xs], Ys) :- N > 1, combination(N,Xs,Ys).

% 1.27 (**) Group the elements of a set into disjoint subsets.
%   a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
%      2, 3 and 4 persons? Write a predicate that generates all the
%      possibilities via backtracking.
% Example:
%   ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
%   G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
%   ...
groupN(1, [X|Xs], [X], Xs).
groupN(1, [X|Xs], Ys, [X|Zs]) :- groupN(1, Xs, Ys, Zs).
groupN(N, [X|Xs], [X|Ys], Zs) :- N > 1, N2 is N-1, groupN(N2, Xs, Ys, Zs).
groupN(N, [X|Xs], Ys, [X|Zs]) :- N > 1, groupN(N, Xs, Ys, Zs).

group3(L, G1, G2, G3) :- 
    length(L, N), 
    N = 9, 
    groupN(2, L, G1, G), 
    groupN(3, G, G2, G3).

%   b) Generalize the above predicate in a way that we can specify a list of
%      group sizes and the predicate will return a list of groups.
% 
% Example:
% ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
% Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
% ...
% 
% Note that we do not want permutations of the group members; i.e.
% [[aldo,beat],...] is the same solution as [[beat,aldo],...]. However, we make
% a difference between [[aldo,beat],[carla,david],...] and
% [[carla,david],[aldo,beat],...].
% 
% You may find more about this combinatorial problem in a good book on discrete
% mathematics under the term "multinomial coefficients".
group(L, [X,Y], [G|[Gs]]) :- length(L,Length), Length is X + Y, groupN(X, L, G, Gs).
group(L, [N1,N2,N3|Ns], [G|Gs]) :- groupN(N1, L, G, Xs), group(Xs, [N2,N3|Ns], Gs).

% 1.28 (**) Sorting a list of lists according to length of sublists
%   a) We suppose that a list (InList) contains elements that are lists
%   themselves. The objective is to sort the elements of InList according to
%   their length. E.g. short lists first, longer lists later, or vice versa.
% Example:
%   ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%   L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
add_key(Xs, N-Xs) :- 
    length(Xs, N).
add_keys([],[]).
add_keys([X|Xs],[P|Ps]) :- 
    add_key(X,P), 
    add_keys(Xs,Ps).
rem_key(_-X, X).
rem_keys([],[]).
rem_keys([P|Ps],[X|Xs]) :- 
    rem_key(P,X), 
    rem_keys(Ps,Xs).
lsort(List,Sorted) :-
    add_keys(List, Keyed),
    keysort(Keyed, SortedByKey),
    rem_keys(SortedByKey, Sorted).
% 
% b) Again, we suppose that a list (InList) contains elements that are lists
% themselves. But this time the objective is to sort the elements of InList
% according to their length frequency; i.e. in the default, where sorting is
% done ascendingly, lists with rare lengths are placed first, others with a
% more frequent length come later.
% 
% Example:
% ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
% L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
% 
% Note that in the above example, the first two lists in the result L have
% length 4 and 1, both lengths appear just once. The third and forth list have
% length 3; there are two list of this length. And finally, the last three
% lists have length 2. This is the most frequent length. 
flat1([],[]).
flat1([[A|As]|Bs], Zs) :-
    flat1(Bs, Cs),
    append([A|As], Cs, Zs).
lfsort(List, List2) :- 
    add_keys(List, Keyed),
    keysort(Keyed, SortedByKey),
    group_pairs_by_key(SortedByKey, KeyedGroups),
    rem_keys(KeyedGroups, Groups),
    lsort(Groups, SortedNested),
    flat1(SortedNested, List2).

% 2.01 (**) Determine whether a given integer number is prime.
%     Example:
%     ?- is_prime(7).
%     Yes
in_range(From, To, _) :- From > To, !, fail.
in_range(From, To, From) :- From =< To.
in_range(From, To, X) :-
    Next is From + 1,
    in_range(Next, To, X).
is_composite(N) :-
    S is sqrt(N),
    in_range(2, S, Factor),
    Remainder is N mod Factor,
    Remainder = 0,
    !.
is_prime(N) :-
    integer(N),
    N > 1,
    \+ is_composite(N).

% 2.02 (**) Determine the prime factors of a given positive integer.
%     Construct a flat list containing the prime factors in ascending order.
%     Example:
%     ?- prime_factors(315, L).
%     L = [3,3,5,7]
prime_factors(N, [N]) :- is_prime(N).
prime_factors(N, [F|Fs]) :-
    integer(N),
    N > 0,
    S is sqrt(N),
    in_range(2, S, F),
    is_prime(F),
    Q is N / F,
    integer(Q),
    !,
    prime_factors(Q, Fs).

% 2.03 (**) Determine the prime factors of a given positive integer (2).
%     Construct a list containing the prime factors and their multiplicity.
%     Example:
%     ?- prime_factors_mult(315, L).
%     L = [[3,2],[5,1],[7,1]]
%
%     Hint: The solution of problem 1.10 may be helpful.
flip_encoded([],[]).
flip_encoded([[A,B]|Xs], [[B,A]|Ys]) :- flip_encoded(Xs, Ys).
prime_factors_mult(N, L) :-
    prime_factors(N, Fs),
    encode(Fs, Gs),
    flip_encoded(Gs, L).

% 2.04 (*) A list of prime numbers.
%     Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
prime_in_range(From, To, N) :-
    in_range(From, To, N),
    is_prime(N).
primes_in_range(From, To, L) :-
    findall(N, prime_in_range(From, To, N), L).

% 2.05 (**) Goldbach's conjecture.
%     Goldbach's conjecture says that every positive even number greater than 2
%     is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
%     most famous facts in number theory that has not been proved to be correct
%     in the general case. It has been numerically confirmed up to very large
%     numbers (much larger than we can go with our Prolog system). Write a
%     predicate to find the two prime numbers that sum up to a given even
%     integer.
%
%     Example:
%     ?- goldbach(28, L).
%     L = [5,23]
is_even(N) :- integer(N), R is N mod 2, R = 0.
goldbach(N, [A,B]) :-
    is_even(N), 
    N > 2,
    Half is N / 2,
    in_range(2, Half, A),
    is_prime(A),
    B is N - A,
    is_prime(B),
    !.

% 2.06 (**) A list of Goldbach compositions.
%     Given a range of integers by its lower and upper limit, print a list of
%     all even numbers and their Goldbach composition.
%
%     Example:
%     ?- goldbach_list(9,20).
%     10 = 3 + 7
%     12 = 5 + 7
%     14 = 3 + 11
%     16 = 3 + 13
%     18 = 5 + 13
%     20 = 3 + 17
%
%     In most cases, if an even number is written as the sum of two prime
%     numbers, one of them is very small. Very rarely, the primes are both
%     bigger than say 50. Try to find out how many such cases there are in the
%     range 2..3000.
%
%     Example (for a print limit of 50):
%     ?- goldbach_list(1,2000,50).
%     992 = 73 + 919
%     1382 = 61 + 1321
%     1856 = 67 + 1789
%     1928 = 61 + 1867
goldbach_list(From, To, Minimum) :-
    in_range(From,To,N), 
    is_even(N), 
    goldbach(N, [A,B]),
    A >= Minimum,
    B >= Minimum,
    writeln(N = A + B),
    fail.
goldbach_list(_,_,_).
goldbach_list(From, To) :- 
    goldbach_list(From, To, 2).

% 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
%     Use Euclid's algorithm.
%     Example:
%     ?- gcd(36, 63, G).
%     G = 9
%
%     Define gcd as an arithmetic function; so you can use it like this:
%     ?- G is gcd(36,63).
%     G = 9
gcd(A, 0, A) :- !.
gcd(A, B, Y) :- B \= 0, X is A mod B, gcd(B, X, Y).
gcd(A, B) := G :- gcd(A, B, G).

% 2.08 (*) Determine whether two positive integer numbers are coprime.
%     Two numbers are coprime if their greatest common divisor equals 1.
%     Example:
%     ?- coprime(35, 64).
%     Yes
coprime(A, B) :- gcd(A, B, 1).

% 2.09 (**) Calculate Euler's totient function phi(m).  Euler's so-called totient
%     function phi(m) is defined as the number of positive integers r (1 <= r < m)
%     that are coprime to m.
%
%     Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
%
%     ?- Phi is totient_phi(10).
%     Phi = 4
%
%     Find out what the value of p, Rhi(m) is if m is a prime number. Euler's
%     totient function plays an important role in one of the most widely used
%     public key cryptography methods (RSA). In this exercise you should use the
%     most primitive method to calculate this function. There is a smarter way
%     that we shall use in 2.10.
in_r_range(From, To, _) :- From >= To, !, fail.
in_r_range(From, _, From).
in_r_range(From, To, X) :-
    Next is From + 1,
    in_range(Next, To, X).
r(M, R) :-
    in_r_range(1, M, R),
    coprime(M, R).
totient_phi(1, 1) :- !.
totient_phi(M, N) :-
    findall(R, r(M, R), Rs),
    length(Rs, N).

% 2.10 (**) Calculate Euler's totient function phi(m) (2).
%     See problem 2.09 for the definition of Euler's totient function. If the
%     list of the prime factors of a number m is known in the form of problem
%     2.03 then the function phi(m) can be efficiently calculated as follows:
%     Let [[p1,m1],[p2,m2],[p3,m3],...] be the list of prime factors (and their
%     multiplicities) of a given number m. Then phi(m) can be calculated with
%     the following formula:
% 
%     phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...
% 
%     Note that a**b stands for the b'th power of a.
%
totient_phi2_loop(Accumulator, [[P|M]], N) :-
    !, N is Accumulator * (P - 1) * (P ** (M - 1)).
totient_phi2_loop(Accumulator, [[P|M]|Tail], N) :-
    !, Next is Accumulator * (P - 1) * (P ** (M - 1)),
    totient_phi2_loop(Next, Tail, N).
totient_phi2(1, 1) :- !.
totient_phi2(M, N) :-
    prime_factors_mult(M, L),
    !, totient_phi2_loop(1, L, N).

% 2.11 (*) Compare the two methods of calculating Euler's totient function.
%     Use the solutions of problems 2.09 and 2.10 to compare the algorithms.
%     Take the number of logical inferences as a measure for efficiency. Try
%     to calculate phi(10090) as an example.
compare_totients :-
    time(totient_phi(10090, Phi)),
    write("totient_phi(10090, Phi) -> Phi = "), write(Phi), nl,
    time(totient_phi2(10090, Phi2)),
    write("totient_phi2(10090, Phi2) -> Phi2 = "), write(Phi2), nl.
