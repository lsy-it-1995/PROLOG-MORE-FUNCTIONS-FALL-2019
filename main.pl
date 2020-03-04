connected(X,Y):-
    door_between(X,Y); door_between(Y,X).

path_from(X,X,[]).
path_from(X,Y,Path):-
    path_from(X, Y, [X], Path).

path_from(Ori, Dest, Visit, [Ori,Dest]):-
    connected(Ori,Dest),
    \+member(Dest,Visit).

path_from(Ori, Dest, Acc, [Ori|Result]):-
    connected(Ori, NewNode),
    \+member(NewNode, Acc),
    path_from(NewNode, Dest, [NewNode|Acc], Result).


gender_check(X,Y):-
    female(X), male(Y);
    male(X), female(Y).

party_seating(L):-
    speaks(X,_),
    party_seating_helper(1, [X], L).

party_seating_helper(Ctr, Acc, Acc):-
    Ctr = 10.

party_seating_helper(Ctr, Acc, R):-
    Ctr = 9,
    get_last(Acc, LastAcc),
    speaks(LastAcc,AccP_L),
    speaks(P, AccP_L),
    gender_check(LastAcc, P),
    get_first(Acc, Head),
    speaks(Head,Head_L),
    speaks(P,Head_L),
    gender_check(Head,P),
    \+(member(P,Acc)),
    append(Acc, [P], NewAcc),
    NewCtr is Ctr + 1, 
    party_seating_helper(NewCtr, NewAcc, R).
    
party_seating_helper(Ctr, Acc, R):-
	Ctr<9,
    get_last(Acc, Acc_P),
    speaks(Acc_P,Acc_P_L),
    speaks(P,Acc_P_L),
    gender_check(P,Acc_P),
    \+(member(P,Acc)),
    append(Acc, [P], NewAcc),
    NewCtr is Ctr + 1, 
    party_seating_helper(NewCtr, NewAcc, R).

get_first([H|_],H).
get_last([H],H). 
get_last([_|T],P):-
    get_last(T,P).         



my_deri(X^N,DF):-
    Deri_exp is N -1,
    New_coef is N*1,
    (Deri_exp = 1->   DF = New_coef*X;
    DF = New_coef*X^Deri_exp),!.
my_deri(X^N+Y,DF):-
    my_deri(X^N,DF),!.
my_deri(F,DF):-
    my_deri(X^N+Y,DF);my_deri(X^N,DF).


%N/N
my_div(N1,N2,R):-
    number(N1), number(N2),
    (N1=0->  R is 0;
    R is N1/N2
    ),!.

%x/Nx
my_div(X,N2*X,R*X):-
    atom(X),
    number(N2),
    R is 1/N2,!.

%y/Nx
my_div(Y,N2*X,R*Y/X):-
    atom(Y),
    number(N2),atom(X),
    R is 1/N2,!.

%Nx/Nx
my_div(N1*X,N2*X,R*X):-
    number(N1),atom(X),
    number(N2),
    R is N1/N2,!.
%0x/Ny
my_div(N1*X,N2*Y,0):-
    X \==Y,
    number(N1),N1 = 0, atom(X),
    number(N2),atom(Y),!.
%0x/Ny
my_div(N1,N2*Y,0):-
    number(N1),N1 = 0,
    number(N2),atom(Y),!.
%Nx/Ny
my_div(N1*X,N2*Y,R*X/Y):-
    X \==Y,
    number(N1),atom(X),
    number(N2),atom(Y),
    R is N1/N2,!.




%0*N; N*0
my_mult(N1,N2,0):-
    number(N1),number(N2),
    N1=0;N2=0,!.

%N*N
my_mult(N1,N2,R):-
    number(N1),
    number(N2),
    R is N1*N2,!.

%Nx*N
my_mult(N1*X,N2,0):-
    number(N1),atom(X),
    number(N2), N2 = 0,!.
%N*Nx
my_mult(N1,N2*X,0):-
    number(N2),atom(X),
    number(N1), N1 = 0,!.

%Nx*N
my_mult(N1*X,N2,R*X):-
    number(N1),atom(X),
    number(N2),
    R is N1*N2,!.
%N*Nx
my_mult(N1,N2*X,R*X):-
    number(N1),
    number(N2),atom(X),
    R is N1*N2,!.
%Nx*Ny
my_mult(N1*X,N2*Y,R*X*Y):-
    number(N1),atom(X),
    number(N2),atom(Y),
    R is N1*N2,!.


%N-N
my_sub(N1,N2,R):-
    number(N1),number(N2),
    R is N1-N2.

%N-x
my_sub(N1, X, N1-X):-
    number(N1),atom(X),!.
%x-N
my_sub(X,N1, X-N1):-
    number(N1),atom(X),!.

%Nx-Nx
my_sub(N1*X, N2*X, N3*X):-
    number(N1), atom(X),
    number(N2),
    N3 is N1 - N2,!.

%Nx-N
my_sub(N1*X, N2,N1*X):-
    number(N1), atom(X),
    number(N2), N2 = 0,!.

%N-Nx
my_sub(N1, N2*X,N2*X):-
    number(N1), N1 = 0,
    number(N2),atom(X),!.

%Nx-N
my_sub(N1*X, N2,N1*X-N2):-
    number(N1), atom(X),
    number(N2),!.

%N-Nx
my_sub(N1,N2*X, N1-N2*X):-
    number(N1),
    number(N2), atom(X),!.

%%Nx-Ny
my_sub(N1*X, N2*Y, N1*X-N2*Y):-
    number(N1), atom(X),
    number(N2), atom(Y),!.

%N+N
my_add(N1,N2,R):-
    number(N1),number(N2),
    R is N1 + N2,!.

%Nx+Nx
my_add(N1*X, N2*X, N3*X):-
    number(N1), number(N2),
    atom(X),
    N3 is N1 + N2,!.

%Nx+0 = Nx
my_add(N1*X, N2,N1*X):-
    number(N1), atom(X),
    number(N2), N2 = 0.

%0+Nx = Nx
my_add(N1, N2*X,N2*X):-
    number(N1), N1 = 0,
    number(N2),atom(X),!.

%Nx+N
my_add(N1*X, N2,N1*X+N2):-
    number(N1), atom(X),
    number(N2),!.

%N+Nx
my_add(N1,N2*X, N2*X+N1):-
    number(N1),
    number(N2), atom(X),!.

%Nx+Ny
my_add(N1*X, N2*Y, N1*X+N2*Y):-
    number(N1), atom(X),
    number(N2), atom(Y),!.




my_der(F,DF):-
    my_der_help(F,DF).

my_der_help(F,0):-
    number(F).

my_der_help(X,1):-
    atom(X).
my_der_help(-X,-1):-
    atom(X).


