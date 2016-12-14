/* spider.pro
 *
 * Descripción: Este archivo contiene la implantación del predicado spider/2.
                El predicado busca todos los grafos araña de un grafo, si los
                tiene.
 * Materia:     Taller de Lenguajes de Programación I (CI-3661)
 * Grupo  :     25
 * Entrega:     Tarea Prolog
 */

%% spider(+Graph:atom, ?Edges:list)
%
%  -Graph: identificador del grafo a examinar.
%  -Edges: lista con las aristas que forman la araña de cobertura.
%
% Este predicado triunfa si el grafo especificado por Graph contiene una 'araña'.
% Si triunfa, Edges queda unificado con la 'araña' encontrada. Se pueden generar
% todas las arañas del grafo.

spider(Graph,Edges) :-
    vertices(Graph,Vertices),
    member(V,Vertices),
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
    N >= 3.

%% vertices(+Graph:atom, ?Vertices:list)
%
%  -Graph: identificador del grafo.
%  -Vertices: lista que contiene a los vertidel grafo.
%
% Este predicado se usa para obtener todos los vértices del grafo Graph.

vertices(Graph,Vertices) :-
    findall(X-Y,edge(Graph,X,Y),V),
    unpair(V,Vertices),
    sort(Vertices).

%% unpair(+Pairs:list, ?Unpaired:list)
%
%  -Pairs: es la lista de pares a separar. Los pares osn de la forma: X-Y.
%  -Unpaired: lista con los pares separados.
%
% Este predicado se usa para separar los pares de la lista pares que produjo
% vertices.

unpair([],[]) :- !.
unpair([X-Y|Rest],[X,Y|L]) :- unpair(Rest,L).

%% clean(+E0:list, ?E1:list)
%
%  -E0: lista de aristas original.
%  -E1: lista que resulta de quitar las repeticiones y los identificadores del
%       grafo de las aristas en E0.
%
% Este predicado se usa para eliminar las aristas repetidas de E0 y para quitar
% el identificador del grafo de cada arista para que se guarden con el formato
% especificado en el enunciado.

clean([],[]) :- !.
clean([edge(_,X,X)|Edges],R) :-
    clean(Edges,R),!.
clean([edge(_,X,Y)|Edges],[edge(X,Y)|R]) :-
    Y\=X,
    clean(Edges,R).
