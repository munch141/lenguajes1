/* spider.pro
 *
 * Descripción: Este archivo contiene la implantación del predicado spider/2.
                El predicado busca todos los grafos araña de un grafo, si los
                tiene.
 * Materia:     Taller de Lenguajes de Programación I (CI-3661)
 * Grupo  :     25
 * Entrega:     Tarea Prolog
 */

%% spider(+Graph:atom, ?T:list)
%
%  -Graph: identificador del grafo a examinar.
%  -T: lista con las aristas que forman la araña de cobertura.
%
% Este predicado triunfa si T contiene un árbol cobertor sobre Graph y además es
% un grafo araña.

spider(Graph,T) :-
    findall(X,(spanning_tree(Graph,X)),SpanTrees),
    sort(SpanTrees),
    vertices(Graph,Vertices),
    member(T,SpanTrees),
    no_spider(T,Vertices).

%% no_spider(+G:list, +Vertices:list)
%
%  -G: lista de aristas que representan un grafo.
%  -Vertices: lista de los vertices del grafo.
%
% Este predicado, junto con spider_found, se usa para verificar si hay más de
% una araña en el grafo. Una araña en este contexto es un vertice del que salen
% 3 o más aristas. Si se consigue una araña, se sigue revisando el grafo pero
% con spider_found.

no_spider(G,[]) :- !.
no_spider(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N >= 3,
    spider_found(G,Vertices),!.
no_spider(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N < 3,
    no_spider(G,Vertices).

%% spider_found(+G:list, +Vertices:list)
%
%  -G: lista de aristas que representan un grafo.
%  -Vertices: lista de los vertices del grafo.
%
% Este predicado se usa para revisar si hay otra arña en el grafo sabiendo que
% ya hay una.

spider_found(G,[]) :- !.
spider_found(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N < 3,
    spider_found(G,Vertices).

%% subtract(+L1:list, +L2:list, ?L3:list)
%
%  -L1: lista de la se van a eliminar elementos.
%  -L2: lista con los elementos a eliminar de L1.
%  -L3: lista con el resultado de eliminar los elementos de L2 que esten en L1.
%
% Este predicado triunfa si L3 es el resultado de eliminar los elementos en L2
% de L1.

subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).

%% children(+V:atom, +Edges:list, ?Children:list)
%
%  -V: vertice padre.
%  -Edges: lista de aristas de la que se quieren sacar los 'hijos' de V.
%  -Children: lista con los vértices hijos de V.
%
% Este predicado triunfa unificando Children con la lista de vértices directamente
% conectados con V en el grafo Edges.

children(_,[],[]) :- !.
children(V,[edge(V,C)|Edges],[C|Children]) :-
    V \= C,
    children(V,Edges,Children),!.
children(V,[edge(C,V)|Edges],[C|Children]) :-
    V \= C,
    children(V,Edges,Children),!.
children(V,[_|Edges],Children) :-
    children(V,Edges,Children),!.

%% edges_from(+V:atom, +Edges:list, ?R:list)
%
%  -V: vértice origen.
%  -Edges: lista de aristas a revisar.
%  -R: lista con las aristas que salen de V.
%
% Este predicado triunfa si R está unificado con la lista de aristas que salen
% o entran de V en Edges.

edges_from(_,[],[]) :- !.
edges_from(V,[edge(V,Y)|Edges],[edge(V,Y)|R]) :-
    edges_from(V,Edges,R),!.
edges_from(V,[edge(X,V)|Edges],[edge(X,V)|R]) :-
    edges_from(V,Edges,R),!.
edges_from(V,[_|Edges],R) :-
    edges_from(V,Edges,R),!.

%% eliminar_varios(+Vertices:list, +G:list, ?R:list)
%
%  -Vertices: lista de vertices a eliminar del grafo.
%  -G: lista de aristas.
%  -R: aristas que resulta de elimnar los vertices en Vertices de G.
%
% Este predicado triunfa si R contiene el resultado de eliminar los vertices en
% Vertices del grafo representado por G.

eliminar_varios(_,[],[]) :- !.
eliminar_varios([],G,G) :- !.
eliminar_varios([Head|Tail],G,G1) :-
    edges_from(Head,G,Children),
    subtract(G,Children,G0),
    eliminar_varios(Tail,G0,G1).

%% conectan(+V:atom, +Vertices:list, +G:list, ?Edges:list)
%
%  -V: vertice origen.
%  -Vertices: lista de vertices.
%  -G: lista de aristas que representan un grafo.
%  -Edges: lista de aristas resultantes.
%
% Este predicado triunfa si R contiene la lista de aristas en G que conectan a
% con los vertices en Vertices. R puede ser la lista vacía.

conectan(_,[],_,[]) :- !.
conectan(_,_,[],[]) :- !.
conectan(V,[Head|Tail],G,[edge(Head,V)|Edges]) :-
    member(edge(Head,V),G),!,
    conectan(V,Tail,G,Edges),!.
conectan(V,[Head|Tail],G,[edge(V,Head)|Edges]) :-
    member(edge(V,Head),G),!,
    conectan(V,Tail,G,Edges),!.
conectan(V,[_|Tail],G,Edges) :-
    conectan(V,Tail,G,Edges).

%% spanning_tree(+Graph:atom, ?T:list)
%
%  -Graph: identificador del grafo a revisar.
%  -T: lista de aristas que representan un árbol cobertor de Graph.
%
% Este predicado se usa para generar todos los árboles de cobertura del grafo
% Graph. Los árboles se dan uno a la vez, usando backtracking se pueden generar
% todos.

spanning_tree(Graph,T) :-
    findall(edge(X,Y),(edge(Graph,X,Y)),G),
    vertices(Graph,Vertices),
    member(Root,Vertices),!,
    delete(Vertices,Root,Rest),
    go(Root,G,Rest,T,_).

%% go(+V:atom, +G:list, +Find:list, ?G_final:list, ?Faltaron:list)
%
%  -V: Vertice desde el que inicia la búsqueda.
%  -G: lista de aristas del grafo que se va a recorrer.
%  -Find: lista de vertices que se van a buscar.
%  -G_final: lista de aristas que representan los caminos encontrados.
%  -Faltaron: lista de vértices que no se pudieron encontrar.
%
% Este predicado genera un árbol cobertor sobre el grafo G. La idea es empezar
% desde un nodo y usar todas las combinaciones de aristas desde ese nodo para
% recorrer el resto del árbol. Primero se elige una combinación de aristas de
% salida desde V, luego se eliminan esas aristas del grafo original, después se
% elige un vértice al que se llege por una de esas aristas y se toma como la
% nueva raíz para buscar los vértices que falta por encontrar.

go(_,[],[],[],[]) :- !.
go(_,[],Find,[],Find) :- !.
go(V,G,Find,G_final,Faltaron) :-
    edges_from(V,G,V_edges),     % aristas que salen de V
    subtract(G,V_edges,G1),      % se eliminan esas aristas del grafo
    % se buscan las partes del conjunto de hijos de V
    findall(L1,(children(V,V_edges,Children),sublist(L1,Children)),L2),
    delete(L2,[],Combs),
    member(Comb,Combs),          % se elige una combinación de aristas de V

    subtract(Find,Comb,Find1),   % se eliminan los vértices á los que se llega
                                 % con la combinación elegida porque ya están
                                 % en el camino

    member(Next,Comb),           % se elige un vértice para que sea la raíz
    delete(Comb,Next,Rest),
    eliminar_varios(Rest,G1,G2), % se eliminan las aristas que salen de los
                                 % vértices encontrados 
    go(Next,G2,Find1,G_final0,Faltaron), % se continua la búsqueda
    Faltaron == [],
    conectan(V,Comb,V_edges,V_graph),
    append(V_graph,G_final0,G_final).

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
