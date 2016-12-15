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




no_spider(G,[]) :- !.
no_spider(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N >= 3,
    spider_found(G,Vertices),!.
no_spider(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N < 3,
    no_spider(G,Vertices).



spider_found(G,[]) :- !.
spider_found(G,[X|Vertices]) :-
    children(X,G,C),
    length(C,N), N < 3,
    spider_found(G,Vertices).



subtract([], _, []).
subtract([Head|Tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        subtract(Tail, L2, L3).
subtract([Head|Tail1], L2, [Head|Tail3]) :-
        subtract(Tail1, L2, Tail3).



children(_,[],[]) :- !.
children(V,[edge(V,C)|Edges],[C|Children]) :-
    V \= C,
    children(V,Edges,Children),!.
children(V,[edge(C,V)|Edges],[C|Children]) :-
    V \= C,
    children(V,Edges,Children),!.
children(V,[_|Edges],Children) :-
    children(V,Edges,Children),!.



edges_from(_,[],[]) :- !.
edges_from(V,[edge(V,Y)|Edges],[edge(V,Y)|R]) :-
    edges_from(V,Edges,R),!.
edges_from(V,[edge(X,V)|Edges],[edge(X,V)|R]) :-
    edges_from(V,Edges,R),!.
edges_from(V,[_|Edges],R) :-
    edges_from(V,Edges,R),!.



eliminar_varios(_,[],[]) :- !.
eliminar_varios([],G,G) :- !.
eliminar_varios([Head|Tail],G,G1) :-
    edges_from(Head,G,Children),
    subtract(G,Children,G0),
    eliminar_varios(Tail,G0,G1).



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



spanning_tree(Graph,T) :-
    findall(edge(X,Y),(edge(Graph,X,Y)),G),
    vertices(Graph,Vertices),
    member(Root,Vertices),!,
    delete(Vertices,Root,Rest),
    go(Root,G,Rest,T,_).



go(_,[],[],[],[]) :- !.
go(_,[],Find,[],Find) :- !.
go(V,G,Find,G_final,Faltaron) :-
    edges_from(V,G,V_edges),
    subtract(G,V_edges,G1),
    findall(L1,(children(V,V_edges,Children),sublist(L1,Children)),L2),
    delete(L2,[],Combs),
    member(Comb,Combs),
    subtract(Find,Comb,Find1),
    member(Next,Comb),
    delete(Comb,Next,Rest),
    eliminar_varios(Rest,G1,G2),
    go(Next,G2,Find1,G_final0,Faltaron),
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
