%입력으로 들어온 값을 그대로 반환하는 함수
mv(LST, LST) :- !.

/*
리스트 ORIGIN의 INDEX번째 원소를 NEW로 바꾸어 
리스트 RET 생성하는 함수
*/
change(INDEX, ORIGIN, NEW, RET) :-
  nth1(INDEX, ORIGIN, _, TMP),
  nth1(INDEX, RET, NEW, TMP).

%COL1 + 1행 J열에 퀸을 배치하는 함수
loop3(COL1, J, MAX2, ANS3, FIN1, RET2) :- 
    COL2 is COL1 + 1,
    change(COL2, ANS3, J, TMP2),
    loop(COL2, MAX2, TMP2, FIN, TMP3),
    FIN =:= 1 -> 
    FIN1 is 1, mv(TMP3, RET2); 
    J =\= MAX2, 
    J2 is J + 1, loop3(COL1, J2, MAX2, ANS3, FIN1, RET2).

%COL행에 놓인 퀸이 배치 가능한 위치에 놓인 것인지 확인하는 함수
visited(COL, TARGET, _, 1) :- 
    TARGET >= COL.
visited(COL, TARGET, LOC, 0) :- 
    TARGET < COL,
    nth1(COL, LOC, V1),
    nth1(TARGET, LOC, V2),
    V1 =:= V2.
visited(COL, TARGET, LOC, 0) :- 
    TARGET < COL,
    nth1(COL, LOC, V1),
    nth1(TARGET, LOC, V2),
    TMP1 is V1 - V2,
    TMP2 is COL - TARGET,
    V3 is abs(TMP1),
    V4 is abs(TMP2),
    V3 =:= V4.
visited(COL, TARGET, LOC, RES) :- 
    TARGET < COL,
    nth1(COL, LOC, V1),
    nth1(TARGET, LOC, V2),
    TMP1 is V1 - V2,
    TMP2 is COL - TARGET,
    V3 is abs(TMP1),
    V4 is abs(TMP2),
    V1 =\= V2, V3 =\= V4,
    TARGET1 is TARGET + 1,
    visited(COL, TARGET1, LOC, RES).

%0번째~(MAX-1)번째 퀸을 순차적으로 배치하기 위해 반복문 구현한 함수
loop(MAX, MAX, ANS1, 1, ANS1) :- 
    visited(MAX, 1, ANS1, RES2),
    RES2 =:= 1.
loop(CUR, MAX, ANS1, FIN2, RES1) :- 
    visited(CUR, 1, ANS1, RES3),
    RES3 =:= 1,
    loop3(CUR, 1, MAX, ANS1, FIN2, RES1).
loop(CUR, _, ANS1, 0, ANS1) :- 
    visited(CUR, 1, ANS1, RES4),
    RES4 =:= 0.

%배치하려는 퀸의 수를 입력받아 배치 결과를 반환하는 함수
n_queen(N, X) :- 
    numlist(1, N, ANS),
    loop(0, N, ANS, _, R),
    reverse(R, X).
