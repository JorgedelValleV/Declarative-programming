% Jorge del VAlle VAzquez

% Ejercicio 1
elimina1([ ],X,[ ]).
elimina1([X|R],Y,NR) :- Y == X, elimina1(R,Y, NR).
elimina1([X|R],Y,[X|NR]) :- Y \== X, elimina1(R,Y,NR).

elimina2([ ],X,[ ]).
elimina2([X|R],Y,NR) :- Y = X, elimina2(R,Y, NR).
elimina2([X|R],Y,[X|NR]) :- Y \= X, elimina2(R,Y,NR).

elimina3([ ],X,[ ]).
elimina3([X|R],X,NR) :- elimina3(R,X,NR).
elimina3([X|R],Y,[X|NR]) :- Y \== X, elimina3(R,Y,NR).

% La ejecucion de eliminai([a,b,a,c],a,L). funciona para todos igual
% Para la ejecucion eliminai([a,b,a,c],X,L). :
% elimina 1 da como resultado la lista entera porque X no unifica e Y==X falla
% elimina 2 elimina las apariciones del primer elemento
% elimina 3 ofrece los resultados de eliminar cada uno de los elementos

% Ejercicio 2

sumatree(nil,0).
sumatree(t(Y,L,R),X):-sumatree(L,X1),sumatree(R,X2),X is Y+X1+X2.

% el maximo de tres elementos
maxx(N1,N2,N3,M):-M is max(N1,max(N2,N3)).

maximo(nil,0).
maximo(arbol(N,I,D),S):-maximo(I,X1),maximo(D,X2),maxx(N,X1,X2,S).




% Ejercicio 3
prefijo([],Ys).
prefijo([X|Xs],[X|Ys]) :- 
  prefijo(Xs,Ys).

sufijo(Xs,Xs).
sufijo(Xs,[_|Ys]) :- sufijo(Xs,Ys).

sublista([],_).
sublista([X|Xs],L) :- prefijo(L1,L),sufijo([X|Xs],L1).


% Ejercicio 4
op(500, xfy, -->).
op(600, xfy, yy).


hanoi(0,_,_,_,_).
hanoi(1,A,B,C,[A,B]).
hanoi(N,A,B,C,M):- 
  N>1,
  N1 is N-1,
  hanoi(N1,A,C,B,M1),
  append(M1,[A,B|M2],M),
  hanoi(N1,C,B,A,M2).

hanoii(0,_,_,_).
hanoii(1,A,B,C):-
  write(A --> B yy),
hanoii(N,A,B,C):- 
  N>1,
  N1 is N-1,
  hanoii(N1,A,C,B),
  hanoii(1,A,B,_):-
  hanoii(N1,C,B,A).


muestraHan(N,A,B,C):-
  hanoi(N,A,B,C,M),
  muestra(M).

muestra([]).
muestra([A,B]):- 
  write(A --> B).
muestra([A,B|Ms]):-
  write(A --> B yy),
  muestra(Ms).









