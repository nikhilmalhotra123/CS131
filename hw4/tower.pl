% Tower
tower(0, [], counts([],[],[],[])) .
tower(N, T, C) :-
  valuesCheck(N, T),
  countEverything(T, C) .

%valuesCheck verfies the 2-D array has N arrays and the rows and columns are transposes of each other. It also sets the domains using finite state.
valuesCheck(N, Rows) :-
  length(Rows, N),
  maplist(within_domain(N), Rows),
  maplist(fd_all_different, Rows),
  length(Columns, N),
  transpose(Rows, Columns),
  maplist(fd_all_different, Columns),
  maplist(fd_labeling, Rows) .

within_domain(N, Row) :-
  length(Row, N),
  fd_domain(Row, 1, N).

%=====================

% Plain Tower
plain_tower(0, [], counts([],[],[],[])) .
plain_tower(N, T, C) :-
  valuesCheckPlain(N, T, C),
  countEverything(T, C) .

%valuesCheckPlain verfies the 2-D array has N arrays and the rows and columns are transposes of each other. It also makes sure the rows fulfil the Left count requirenments before checking the columns at all
valuesCheckPlain(N, Rows, counts(_, _, Left, _)) :-
  length(Rows, N),
  length(L, N),
  do_list(N, L),
  length(Columns, N),
  transpose(Rows, Columns), !,
  maplist(row_init(N, L), Rows, Left),
  maplist(col_init(N, L), Columns) .

do_list(N, L) :-
  findall(Num, between(1, N, Num), L) .

row_init(N, L, Row, Left) :-
  length(Row, N), !,
  permutation(L, Row),
  count(Row, Left) .

col_init(N, L, Row) :-
  length(Row, N), !,
  permutation(L, Row).

% ======================

% The following predicates make sure the values of T match with the counts C
countEverything(Rows, counts(Top, Bottom, Left, Right)) :-
  countLefts(Rows, Left),
  countRights(Rows, Right),
  countTops(Rows, Top),
  countBottoms(Rows, Bottom) .

count(Row, C) :-
  countRow(Row, C, 0) .

countRow([], 0, _) .
countRow([X|Y], C, Max) :-
  X > Max,
  countRow(Y, P, X),
  C is (P + 1) .

countRow([X|Y], C, Max) :-
  X < Max,
  countRow(Y, C, Max) .

countLefts([], []) .
countLefts([Row|Rows], [C|Counts]) :-
  count(Row, C),
  countLefts(Rows, Counts) .

countRights(Rows, Counts) :-
  reverseRows(Rows, Reverse),
  countLefts(Reverse, Counts) .

reverseRows([], []) .
reverseRows([Row|Rows], [Reverse|Reverses]) :-
  reverse(Row, Reverse),
  reverseRows(Rows, Reverses) .

countTops(Rows, Counts) :-
  transpose(Rows, Columns),
  countLefts(Columns, Counts) .

countBottoms(Rows, Counts) :-
  transpose(Rows, Columns),
  countRights(Columns, Counts) .

% This is SWI-prolog's old implementation
% https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

ambiguous(N, C, T1, T2) :- tower(N, T1, C), tower(N, T2, C), T1 \= T2.

speedup(Speed) :-
  statistics(cpu_time, [Start|_]),
  tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  statistics(cpu_time, [End|_]),
  Tow is (End - Start)/5,
  statistics(cpu_time, [Start2|_]),
  plain_tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  plain_tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  plain_tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  plain_tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  plain_tower(4, _, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])),
  statistics(cpu_time, [End2|_]),
  Pla is (End2 - Start2)/5,
  Speed is Pla/Tow.

% =============
% Test cases
/*
tower(5, [[2,3,4,5,1], [5,4,1,3,2], [4,1,5,2,3], [1,2,3,4,5], [3,5,2,1,4]], C).

tower(5, T, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])).

tower(5,[[2,3,4,5,1],[5,4,1,3,2],Row3,[RC41,5|Row4Tail],Row5],counts(Top,[4|BottomTail],[Left1,Left2,Left3,Left4,5],Right)).

plain_tower(4, T, counts([2,2,1,3],[1,3,3,2],[2,2,3,1],[2,3,1,2])).
*/
