/* mk.pro
 *
 * Descripción: Este archivo contiene la implantación del predicado arre/3 que,
 *              dado un entero N, calcula el número máximo de caballos que
 *              pueden ponerse en un tablero de ajedrez de NxN sin que se
 *              ataquen entre si, además produce una lista con las posiciones de
 *              los caballos. Y contiene la impĺantación del predicado caballito/2
 *              que dibuja un tablero con caballos en pantalla.
 * Materia:     Taller de Lenguajes de Programación I (CI-3661)
 * Grupo  :     25
 * Entrega:     Tarea Prolog
 */


%% arre(+N:int, ?Max:int, ?Solucion:list)
%
%  -N: número de filas/columnas del tablero.
%  -Max: número máximo de caballos que se pueden poner en el tablero sin que se
%        ataquen entre sí.
%  -Solucion: lista con las posiciones de los caballos en una solución. Es de
%             la forma: [k(1,1),k(1,2),...,k(i,j)].
%
% Este predicado triunfa al unificar Max con el número máximo de caballos que
% se pueden poner en un tablero de ajedrez de NxN sin que se ataquen entre sí,
% y al unificar Solucion con la lista de las posiciones de los caballos. Si hay
% más de una manera de poner Max caballos en el tablero, se generan todas por
% backtracking.

arre(N,Max,Solucion) :- go(N,0,Max,Solucion).

%% go(+N:int, +M:int, ?Max:int, ?Piezas:list)
%
%  -N: número de filas/columnas del tablero.
%  -M: número de caballos a poner en el tablero para probar.
%  -Max: número máximo de caballos que se pueden poner en el tablero.
%  -Piezas: posiciones de los caballos puestos en el tablero.
%
% Este predicado se usa para ir preguntando si es posible poner M caballos en
% el tablero sin que se ataquen entre sí, hasta que se llegue a un M' en el que
% no es posible. La idea es probar desde 0 hasta llegar al máximo. Si falla es
% porque llegó a Max+1, luego se generan las soluciones para Max.

go(N,M,Max,Piezas) :-
    posible(M,N,_),!,
    M1 is M+1,
    go(N,M1,Max,Piezas).
go(N,M,Max,Piezas) :-
    Max is M-1,
    posible(Max,N,Piezas).

%% posible(+M:int, +N:int, ?L:list)
%
%  -M: cantidad de caballos a poner en el tablero.
%  -N: número de filas/columnas del tablero.
%  -L: lista con las posiciones de caballos.
%
% Este predicado triunfa si es posible poner M caballos en un tablero de NxN
% sin que ningún caballo esté amenazado. Al triunfar, la lista L estará unificada
% con la lista de posiciones de los caballos.

posible(M,N,L) :-
    Tam is N*N,
    M =< Tam,
    gen(Tam,Todos),
    way(N,M,L,Todos,_).

%% way(+N:int, +M:int, ?L:list, +Todos:list, ?P:int)
%
%  -N: número'de filas/columnas del tablero.
%  -M: cantidad de caballos a poner en el tablero.
%  -L: lista con las posiciones de los caballos.
%  -Todos: lista con todas las posiciones del tablero.
%  -P: última posición donde se puso un caballo.
%
% Este predicado se usa para probar todas las combinaciones de M caballos en el
% tablero sin repeticiones. La idea es que un nodo en árbol de llamadas solo pueda
% poner caballos en posiciones que estén después de la posición que eligió su
% nodo hijo, para esto se le quitan todas las posiciones desde 1 hasta P a la
% lista con todas las posiciones (Todos) y se elige la primera posición de la
% lista resultante. En este predicado, las posiciones son enteros, para guardarlas
% en la lista L se transforman en coordenadas primero.

way(_N,0,[],_,_) :- !.
way(_N,1,[k(X,Y)],Todos,P) :-
    member(P,Todos),
    coordenadas(_N,P,X,Y).
way(_,1,_,_,-1) :- !,fail,!.
way(_N,M,[k(X,Y)|Sol],Todos,P) :-
    M1 is M-1,
    way(_N,M1,Sol,Todos,P0),
    gen(P0,Pre),
    append(Pre,Rest,Todos),
    member(P,Rest),
    coordenadas(_N,P,X,Y),
    noattack(k(X,Y),Sol).

%% coordenadas(+N:int, +P:int, ?X:int, ?Y:int)
%
%  -N: número de columnas del tablero.
%  -P: posición en el tablero a transformar en coordenadas.
%  -X: número de fila de P.
%  -Y: número de columna de P.
%
% Este predicado se usa para tranformar un entero P a coordenadas en un tablero
% de NxN.

coordenadas(N,P,X,Y) :-
    P1 is P-1,
    X is P1 // N+1,
    Y is P1 mod N+1.

%% noattack(+k(R/C):int/int, +L:list)
%
% Este predicado triunfa si el caballo en la posición R/C no ataca a ningún 
% caballo de la lista L.

noattack(_,[]).
noattack(k(R,C), [k(R1,C1)|Otras]) :-
    (abs(C-C1) =\= 2; abs(R-R1) =\= 1),
    (abs(C-C1) =\= 1; abs(R-R1) =\= 2),
    noattack( k(R,C), Otras ), !.

%% gen(+N:int, ?L:list)
%
%  -N: cantidad de elementos de la lista.
%  -L: lista resultante.
%
%  Este predicado se usa para generar una lista desde 1 hasta N.

gen(N,L) :- gen(N,[],L).
gen(0,A,A) :- !.
gen(N,A,R) :- N > 0, N1 is N-1, gen(N1,[N|A],R).

%% caballito(+N:int, +L:list)
%
%  -N: número de filas/columnas del tablero.
%  -L: lista con las posiciones de los caballos.
%
% Este predicado se usa para dibujar un tablero de NxN con caballos en las posiciones
% de la lista L.

caballito(N,L) :-
    nonvar(N),
    nonvar(L),
    0 =< N,
    pos_validas(N,L),
    sort(L,L1),
    draw(N,0,L1).

%% pos_validas(+N:list, +L:list)
%
%  -N: número de filas/columnas del tablero.
%  -L: lista de posiciones a revisar.
%
% Este predicado triunfa si la lista L no tiene posciones inválidas, esto es,
% si para todo par k(i,j) en L se cumple que 1 <= i <= n y 1 <= j <= n.

pos_validas(N,[]) :- !.
pos_validas(N,[k(X,Y)|Otras]) :-
    1 =< X, X =< N,
    1 =< Y, Y =< N,
    pos_validas(N,Otras).

%% draw(+N:int, +P:int, +L:list)
%
%  -N: número de filas/columnas del tablero.
%  -P: posición actual.
%  -L: lista con las posiciones de los caballos.
%
% Este predicado se usa para dibujar el tablero posición por posición.

draw(N,P,_) :- P =:= N*N+1,!.
draw(N,0,L) :- horiz(N),draw(N,1,L),!.
draw(N,P,[k(_X,N)|Otras]) :-
    coordenadas(N,P,_X,N),
    write('|K|'),nl,
    horiz(N),
    P1 is P+1,
    draw(N,P1,Otras),!.
draw(N,P,Otras) :-
    coordenadas(N,P,_,N),
    write('| |'),nl,
    horiz(N),
    P1 is P+1,
    draw(N,P1,Otras),!.
draw(N,P,[k(_X,Y)|Otras]) :-
    coordenadas(N,P,_X,Y),
    write('|K'),
    P1 is P+1,
    draw(N,P1,Otras),!.
draw(N,P,Otras) :-
    write('| '),
    P1 is P+1,
    draw(N,P1,Otras),!.

%% horiz(+N:int)
%
%  -N: cantidad de columnas del tablero.
%
% Este predicado se usa para dibujar las líneas horizontales del tablero.

horiz(0) :- write('+'), nl.
horiz(N) :- N > 0, write('+-'), N1 is N-1, horiz(N1).
