/* spider.pro
 *
 * Descripción: 
 * Materia:     Taller de Lenguajes de Programación I (CI-3661)
 * Grupo  :     25
 * Entrega:     Tarea Prolog
 */

spider(Graph,Edges) :-
    vertices(Graph,Vertices),
    member(V,Vertices),
    edges_from(Graph,V,Edges).

vertices(Graph,Vertices) :-
    findall(X-Y,edge(Graph,X,Y),V),
    unpair(V,Vertices),
    sort(Vertices).

unpair([],[]) :- !.
unpair([X-Y|Rest],[X,Y|L]) :- unpair(Rest,L).

edges_from(Graph,V,Edges) :-
    findall(
        edge(Graph,X,Y),
        (
            (edge(Graph,X,V), Y = V);
            (edge(Graph,V,Y), X = V)
        ),
        E0
    ),
    clean(E0,Edges),
    length(Edges,N),
    N >= 3,!.

clean([],[]) :- !.
clean([edge(_,X,X)|Edges],R) :-
    clean(Edges,R),!.
clean([edge(G,X,Y)|Edges],[edge(X,Y)|R]) :-
    Y\=X,
    clean(Edges,R).
