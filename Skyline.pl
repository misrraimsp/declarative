%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cabecera del programa Skyline.pl
%% Práctica de Teoría de los Lenguajes de Programación
%% Curso 2015-2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado principal de la práctica,
% verdad si 'Skyline' es la lista de coordenadas correspondiente al skyline de la lista de edificios 'Edificios'.

resuelveSkyline([],[]) :- !.
resuelveSkyline([Edificio],Skyline) :- edificioAskyline(Edificio,Skyline), !.
resuelveSkyline(Edificios,Skyline) :- divide(Edificios,E1,E2), resuelveSkyline(E1,S1), resuelveSkyline(E2,S2), combina(S1,S2,Skyline).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado verdadero si el segundo argumento son las coordenadas del skyline del edificio pasado como primer argumento.

edificioAskyline(ed(X1,X2,H), [c(X1,H),c(X2,0)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado verdadero si el segundo y tercer argumento representan mitades del primer argumento, o a lo sumo con un elemento de diferencia.

divide([],[],[]) :- !.
divide([X],[X],[]) :- !.
divide([A,B|T],[A|T1],[B|T2]) :- divide(T,T1,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado verdadero si 'R' es el skyline resultado de combinar los skylines 'X' e 'Y'.

combina(X,Y,R) :- combina2(X,Y,0,0,0,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar que lleva a la práctica el trabajo del predicado 'combina'. 9 Casos diferentes.

%%% CASO 0: caso base 0
combina2([],[],_,_,_,[]) :- !.

%%% CASO 1: caso base 1
combina2(X,[],_,_,_,X) :- !.

%%% CASO 2: caso base 2
combina2([],X,_,_,_,X) :- !.

%%% CASO 3: X1 menor X2, Altura Redundante. Se poda el árbol para evitar que unifique y recorra la rama del CASO 4
combina2([c(X1,Y1)|T1],[c(X2,Y2)|T2],_,H2,Hactual,T) :- X1 < X2, maximo2(Y1,H2,Hactual), !, combina2(T1,[c(X2,Y2)|T2],Y1,H2,Hactual,T).

%%% CASO 4: X1 menor X2, Altura Válida
combina2([c(X1,Y1)|T1],[c(X2,Y2)|T2],_,H2,_,[c(X1,Altura)|T]) :- X1 < X2, maximo2(Y1,H2,Altura), combina2(T1,[c(X2,Y2)|T2],Y1,H2,Altura,T).

%%% CASO 5: X2 menor X1, Altura Redundante. Se poda el árbol para evitar que unifique y recorra la rama del CASO 6
combina2([c(X1,Y1)|T1],[c(X2,Y2)|T2],H1,_,Hactual,T) :- X2 < X1, maximo2(Y2,H1,Hactual), !, combina2([c(X1,Y1)|T1],T2,H1,Y2,Hactual,T).

%%% CASO 6: X2 menor X1, Altura Válida
combina2([c(X1,Y1)|T1],[c(X2,Y2)|T2],H1,_,_,[c(X2,Altura)|T]) :- X2 < X1, maximo2(Y2,H1,Altura), combina2([c(X1,Y1)|T1],T2,H1,Y2,Altura,T).

%%% CASO 7: X1 igual X2, Altura Redundante. Se poda el árbol para evitar que unifique y recorra la rama del CASO 8
combina2([c(X,H1)|T1],[c(X,H2)|T2],_,_,Hactual,T) :- maximo2(H1,H2,Hactual), !, combina2(T1,T2,H1,H2,Hactual,T).

%%% CASO 8: X1 igual X2, Altura Válida
combina2([c(X,H1)|T1],[c(X,H2)|T2],_,_,_,[c(X,Altura)|T]) :- maximo2(H1,H2,Altura), combina2(T1,T2,H1,H2,Altura,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado que presenta en pantalla el dibujo del skyline pasado como argumento.

dibujaSkyline(Skyline) :- son_las_alturas(Skyline,0,0,Alturas), maximoN(Alturas,Hmax), escribe(Alturas,0,Hmax).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar de 'dibujaSkyline',
% verdad si el último argumento representa la lista de alturas asociada al skyline pasado como primer argumento.

son_las_alturas([],_,_,[]) :- !.
son_las_alturas([c(X,H)|Cs],X,_,[H|As]) :- !, Xsiguiente is X + 1, son_las_alturas(Cs,Xsiguiente,H,As).
son_las_alturas(Cs,Xactual,Hactual,[Hactual|As]) :- Xsiguiente is Xactual + 1, son_las_alturas(Cs,Xsiguiente,Hactual,As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar de 'dibujaSkyline',
% encargado de sacar por pantalla los caracteres que representan el skyline asociado a la lista de alturas pasada como primer parámetro.
% 6 casos diferentes.

%%% CASO 1: caso base, fin
escribe(Alturas,L,0) :- es_la_longitud(Alturas,L), !, nl.

%%% CASO 2: fin de línea, escribe nueva línea
escribe(Alturas,L,Hactual) :- es_la_longitud(Alturas,L), !, H is Hactual - 1, nl, escribe(Alturas,0,H).

%%% CASO 3: última línea, escribe guón
escribe(Alturas,Xactual,0) :- !, X is Xactual + 1, write(-), escribe(Alturas,X,0).

%%% CASO 4: escribe espacio
escribe(Alturas,Xactual,Hactual) :- es_el_elemento(Alturas,Xactual,H), Hactual > H, !, X is Xactual + 1, write(' '), escribe(Alturas,X,Hactual).

%%% CASO 5: escribe asterisco
escribe(Alturas,Xactual,Hactual) :- es_el_elemento(Alturas,Xactual,H), H >= Hactual, X is Xactual + 1, write(*), escribe(Alturas,X,Hactual).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar,
% verdad si el tercer argumento es el máximo entre el primer y el segundo argumento.

maximo2(X,X,X) :- !.
maximo2(X,Y,X) :- X > Y, !.
maximo2(X,Y,Y) :- Y > X.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar,
% verdad si el segundo argumento es el máximo de la lista pasada como primer argumento.

maximoN([M],M) :- !.
maximoN([Cabeza|Cola],M) :- maximoN(Cola,Mactual), maximo2(Cabeza,Mactual,M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar,
% verdad si el tercer argumento se encuentra en la posición dada por el segundo argumento dentro de la lista dada como primer argumento,
% entendiendo que la cabeza de la lista ocupa la posición '0'.

es_el_elemento([X|_],0,X) :- !.
es_el_elemento([_|T],N,X) :- N1 is N - 1, es_el_elemento(T,N1,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicado auxiliar,
% verdad si el segundo argumento es la longitud de la lista pasada en el primer argumento.

es_la_longitud([],0).
es_la_longitud([_|T],L) :- es_la_longitud(T,L1), L is L1 + 1.