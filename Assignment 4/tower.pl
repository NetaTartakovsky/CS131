% ------------------------
% Code for transpose found on https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog

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

% ------------------------
% helper functions

rev_length(N,T) :- length(T,N).
rev_domain(N,List) :- fd_domain(List,1,N).

calculate([],_,Count) :- Count is 0.
calculate([H|T],MaxVal,Count) :-
    MaxVal >= H,
    calculate(T,MaxVal,Count).
calculate([H|T],MaxVal,Cnt) :-
    MaxVal < H,
    calculate(T,H,Count),
    Cnt is Count+1.

% ------------------------
% tower implementation

check([],[]).
check([H|T],[TH|TT]) :-
    calculate(TH,0,X),
    H #= X,
    check(T,TT).

rev_check([],[]).
rev_check([H|T],[TH|TT]) :-
    reverse(TH,TR),
    calculate(TR,0,X),
    H #= X,
    rev_check(T,TT).

tower(N, T, counts(Top,Bottom,Left,Right)) :-
    length(T,N),
    maplist(rev_length(N),[Top,Bottom,Left,Right]),
    maplist(rev_length(N),T),
    maplist(rev_domain(N),T),
    maplist(fd_all_different,T),
    transpose(T,T2),
    maplist(fd_all_different,T2),
    maplist(fd_labeling,T),
    check(Left,T),
    rev_check(Right,T),
    check(Top,T2),
    rev_check(Bottom,T2),
    maplist(fd_labeling,[Top,Bottom,Left,Right]).

% ------------------------
% plain tower implementation

p_domain(0,[]) :- !.
p_domain(N,[N|H]) :-
    M is N-1,
    p_domain(M,H).

p_check([],[]).
p_check([TH|TT],[X|Y]) :-
    calculate(TH,0,X),
    p_check(TT,Y).

p_rev_check([],[]).
p_rev_check([TH|TT],[X|Y]) :-
    reverse(TH,TR),
    calculate(TR,0,X),
    p_rev_check(TT,Y).

p_labeling(_,[],[]).
p_labeling(D,[H|T],[TH|TT]) :-
    permutation(D,H),
    permutation(D,TH),
    p_labeling(D,T,TT).

plain_tower(N, T, counts(Top,Bottom,Left,Right)) :-
    length(T,N),
    maplist(rev_length(N),T),
    transpose(T,T2),
    p_domain(N,D),
    p_labeling(D,T,T2),
    maplist(rev_length(N),[Top,Bottom,Left,Right]),
    p_check(T,Left),
    p_rev_check(T,Right),
    p_check(T2,Top),
    p_rev_check(T2,Bottom).

% ------------------------
% speedup implementation

run_tower(Time) :-
statistics(cpu_time,_),
tower(5, _, counts([3,2,2,5,1],[2,3,4,1,3],[3,2,2,1,2],[1,2,4,3,2])),
statistics(cpu_time,X),
nth(2,X,Time).

run_plain_tower(Time) :-
statistics(cpu_time,_),
plain_tower(5, _, counts([3,2,2,5,1],[2,3,4,1,3],[3,2,2,1,2],[1,2,4,3,2])),
statistics(cpu_time,X),
nth(2,X,Time).
    
speedup(Ratio) :-
run_tower(T),
run_plain_tower(P),
Ratio is P/T.

% ------------------------
% ambiguous implementation

ambiguous(N,C,T1,T2) :-
tower(N,T1,C),
tower(N,T2,C),
T1 \== T2.
