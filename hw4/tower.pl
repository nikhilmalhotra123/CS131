tower(0, [], counts([],[],[],[])) .
tower(N, T, C) :- valuesCheck(N, T), countEverything(T, C).

%valuesCheck verfies the 2-D array has N arrays and the rows and columns are transposes of each other
% Check this base
valuesCheck(_, [[]]) .
valuesCheck(N, Rows) :- length(Rows, N), transpose(Rows, Columns), checkRows(N, Rows), checkRows(N, Columns).

%checkRows initializes a 2D array with rows of length N and unique values from 1..N in each Row
checkRows(_, []) .
checkRows(N, [Row|Rows]) :- length(Row, N), fd_domain(Row, 1, N), fd_labeling(Row), fd_all_different(Row), checkRows(N, Rows) .

countEverything(Rows, [Top, Bottom, Left, Right]) :- countLefts(Rows, Left), countRights(Rows, Right), countTops(Rows, Tops), countBottoms(Rows, Bottoms) .

count(Row, C) :- countRow(Row, C, 0) .

countRow([], 0, _) .
countRow([X|Y], C, Max) :- X > Max, countRow(Y, P, X) , C is (P + 1), ! .
countRow([X|Y], C, Max) :- X < Max, countRow(Y, C, Max), ! .

countLefts([], []) .
countLefts([Row|Rows], [C|Counts]) :- count(Row, C), countLefts(Rows, Counts) .
countRights(Rows, Counts) :- reverse(Rows, Reverse), countLefts(Reverse, Counts) .
countTops(Rows, Counts) :- transpose(Rows, Columns), countRights(Columns, Counts) .
countBottoms(Rows, Counts) :- transpose(Rows, Columns), countLefts(Columns, Counts) .

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
/*
TODO:


*/

% tower(5, [[2,3,4,5,1], [5,5,1,3,2], [4,1,5,2,3], [1,2,3,4,5], [3,5,2,1,4]], C).
