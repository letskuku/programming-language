%리스트의 길이 구하는 함수
len([], 0) :- !.
len([_|LST1], R1) :- 
    len(LST1, R),
    R1 is R + 1.

/*
정렬하려는 원소를 리스트에서 그보다 앞에 놓인 원소들과 비교하여 
올바른 위치에 삽입하는 함수
*/
isort([], TARGET2, [TARGET2]) :- !.
isort(LST3, TARGET2, RET) :- 
    nth0(0, LST3, TARGET1, _),
    TARGET1 >= TARGET2,
    append([TARGET2], LST3, RET).
isort(LST3, TARGET2, RET) :- 
    nth0(0, LST3, TARGET1, EXTRA1),
    TARGET1 < TARGET2,
    isort(EXTRA1, TARGET2, RET1),
    append([TARGET1], RET1, RET).

/*
정렬할 리스트의 앞에서부터 하나씩 삽입 정렬이 이루어지기 위한 반복을 
구현한 함수
*/
loop(LST2, [], MAX, MAX, LST2) :- !.
loop(COM, MOV, NUM, MAX, RES) :- 
    nth0(0, MOV, TARGET, EXTRA),
    isort(COM, TARGET, TMP),
    NUM1 is NUM + 1,
    loop(TMP, EXTRA, NUM1, MAX, RES).

%정렬할 리스트 입력받는 함수
sorting([H|LST], X) :- 
    len([H|LST], L),
    loop([H], LST, 1, L, X).
