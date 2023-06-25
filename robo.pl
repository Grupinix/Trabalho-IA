% Inicia matriz com linhas e colunas definidas pelo usuário, as sujeiras ou obstáculos são adicionados pelo "s" e pelo "o", respectivamente.
% Eles serão adicionados em situações randômicas, enquanto os lugares limpos são representados por "l".
%custo
%Limpo, 1 custo
%Sujeira, 2 custos
%Obstáculos, 3 custos
custo('l',1).
custo('s',2).
custo('o',-1).


obstaculo('o').


inicia_sala(Matriz, Linhas, Colunas,Sala) :-
    length(Matriz, Linhas),
    inicia_linhas(Matriz, Colunas),
    move_robo(Matriz, 0, 0, SalaParcial),
    LinhaFinal is Linhas -1,
    ColunaFinal is Colunas -1,
    substituir_char(SalaParcial, LinhaFinal, ColunaFinal, Sala,'f').

inicia_linhas([], _).
inicia_linhas([Linha | Corpo], Colunas) :-
    length(Linha, Colunas),
    preenche_sala(Linha),
    inicia_linhas(Corpo, Colunas).

define_valor(Resultado) :-
    random(0, 3, Escolha),
    (Escolha = 0 -> Resultado = 'l' ;
     Escolha = 1 -> Resultado = 's' ;
     Resultado = 'o').

preenche_sala([]).
preenche_sala([Cabeca | Corpo]) :-
    define_valor(Valor),
    Cabeca = Valor,
    preenche_sala(Corpo).
%----------------------------Mover-------------------------------------
substituir_char(Sala, X, Y, NovaSala,Char) :-
    nth0(X, Sala, Linha), %Quero pegar a linha na posição X da Sala 
    replace(Y, Linha, Char, NovaLinha), %Faco o replace apenas nessa linha, na posicao Y pelo r, que é o robo
    replace(X, Sala, NovaLinha, NovaSala). %Aplico o replace novamente na matriz, trocando a posição das linhas

%----------------------------Robô--------------------------------------
% Movimento do robô
move_robo(Sala, X, Y, NovaSala) :-
   substituir_char(Sala,X,Y,NovaSala,'r').

limpa_posicao_antiga(Sala, XAntigo, YAntigo, NovaSala):-
    substituir_char(Sala,XAntigo,YAntigo,NovaSala,'l').

replace(Indice, Lista, Elemento, NovaLista) :-
    nth0(Indice, Lista, _, Temporaria),
    nth0(Indice, NovaLista, Elemento, Temporaria).

    
posicao_valida(Sala, NovoX, NovoY, Resposta) :-
    length(Sala, Linhas),
    nth0(NovoX, Sala, Linha),
    nth0(NovoY, Linha, Elemento),
    length(Linha, Colunas),
    ( NovoX >= Linhas -> Resposta = false ;
    NovoX < 0 -> Resposta = false ;
    NovoY >= Colunas -> Resposta = false ;
    NovoY < 0 -> Resposta = false ;
    obstaculo(Elemento) ->  Resposta = false;
    Resposta = true).


d(Sala, XAtual, YAtual, NovaSala):- %Direita
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoY is YAtual+1,
    move_robo(SalaTemporaria, XAtual, NovoY, NovaSala).

e(Sala, XAtual, YAtual, NovaSala):- %Esquerda
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoY is YAtual-1,
    move_robo(SalaTemporaria, XAtual, NovoY, NovaSala).

f(Sala, XAtual, YAtual, NovaSala):- %Frente
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoX is XAtual+1,
    move_robo(SalaTemporaria, NovoX, YAtual, NovaSala).

t(Sala, XAtual, YAtual, NovaSala):- %Tras
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoX is XAtual-1,
    move_robo(SalaTemporaria, NovoX, YAtual, NovaSala).

df(Sala, XAtual, YAtual, NovaSala):- %Diagonal frente
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoX is XAtual+1,
    NovoY is YAtual+1,
    move_robo(SalaTemporaria, NovoX, NovoY, NovaSala).

dt(Sala, XAtual, YAtual, NovaSala):- %Diagonal 
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoX is XAtual-1,
    NovoY is YAtual-1,
    move_robo(SalaTemporaria, NovoX, NovoY, NovaSala).
%----------------------------------Custos----------------------------------
verifica_custo(X,Y,Sala,Custo):-
    nth0(X, Sala, Linha),
    nth0(Y, Linha, Elemento),
    custo(Elemento,Custo).
    
calcula_custo_d(X,Y,Sala,Custo):-
    NovoY is Y+1,
    posicao_valida(Sala, X, NovoY, Resposta),
    ( Resposta = true -> verifica_custo(X,NovoY,Sala,Custo) ; Custo = -1).

calcula_custo_e(X,Y,Sala,Custo):-
    NovoY is Y-1,
    posicao_valida(Sala, X, NovoY, Resposta),
    ( Resposta = true -> verifica_custo(X,NovoY,Sala,Custo) ; Custo = -1).

calcula_custo_f(X,Y,Sala,Custo):-
    NovoX is X+1,
    posicao_valida(Sala, NovoX, Y, Resposta),
    ( Resposta = true -> verifica_custo(NovoX,Y,Sala,Custo) ; Custo = -1).

calcula_custo_t(X,Y,Sala,Custo):-
    NovoX is X-1,
    posicao_valida(Sala, NovoX, Y, Resposta),
    ( Resposta = true -> verifica_custo(NovoX,Y,Sala,Custo) ; Custo = -1).

calcula_custo_df(X,Y,Sala,Custo):-
    NovoX is X+1,
    NovoY is Y+1,
    posicao_valida(Sala, NovoX, NovoY, Resposta),
    ( Resposta = true -> verifica_custo(NovoX,NovoY,Sala,Custo) ; Custo = -1).

calcula_custo_dt(X,Y,Sala,Custo):-
    NovoX is X-1,
    NovoY is Y-1,
    posicao_valida(Sala, NovoX, NovoY, Resposta),
    ( Resposta = true -> verifica_custo(NovoX,NovoY,Sala,Custo) ; Custo = -1).


%----------------------------------- Algorítmos e Heurísticas----------------
objetivo(Estado,Sala) :-
    nth0(0, Estado, X),
    nth0(1, Estado, Y),
    nth0(X, Sala, Linha),
    nth0(Y, Linha, Elemento),
    Elemento == 'f'.

expandir(Sala,Posicao,Sucessores,NovosSucessores):-
    expandir_d(Sala,Posicao,Sucessores,NovosSucessores).

expandir_d(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoY is Y+1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    concat(Sucessores, [X, NovoY], Sucessores1),
    expandir_e(Sala, [X, Y], Sucessores1, NovosSucessores).
expandir_d(Sala, [X, Y], Sucessores, NovosSucessores) :-
    expandir_e(Sala, [X, Y], Sucessores, NovosSucessores).

expandir_e(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoY is Y-1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    concat(Sucessores, [X, NovoY], Sucessores1),
    expandir_f(Sala, [X, Y], Sucessores1, NovosSucessores).
expandir_e(Sala, [X, Y], Sucessores, NovosSucessores) :-
    expandir_f(Sala, [X, Y], Sucessores, NovosSucessores).

expandir_f(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoX is X+1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
    concat(Sucessores, [NovoX, Y], Sucessores1),
    expandir_t(Sala, [X, Y], Sucessores1, NovosSucessores).
expandir_f(Sala, [X, Y], Sucessores, NovosSucessores) :-
    expandir_t(Sala, [X, Y], Sucessores, NovosSucessores).

expandir_t(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoX is X-1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
    concat(Sucessores, [NovoX, Y], Sucessores1),
	expandir_df(Sala, [X, Y], Sucessores1, NovosSucessores).
expandir_t(Sala, [X, Y], Sucessores, NovosSucessores) :-
    expandir_df(Sala, [X, Y], Sucessores, NovosSucessores).

expandir_df(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
    concat(Sucessores, [NovoX, NovoY], Sucessores1),
	expandir_dt(Sala, [X, Y], Sucessores1, NovosSucessores).
expandir_df(Sala, [X, Y], Sucessores, NovosSucessores) :-
    expandir_dt(Sala, [X, Y], Sucessores, NovosSucessores).

expandir_dt(Sala, [X, Y], Sucessores, NovosSucessores) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
    concat(Sucessores, [NovoX, NovoY], NovosSucessores).

expandir_dt(_,_, Sucessores, Sucessores).

concat([], L, [L]).
concat([X | L1], L2, [X | L3]) :-
    concat(L1, L2, L3).
%---------------------------- Busca Profundidade--------------------------
busca_profundidade(Estado, Caminho, Sala) :-
    busca_profundidade_aux(Estado, [], CaminhoReverso, Sala),
	reverse(CaminhoReverso,Caminho).

busca_profundidade_aux(Estado, CaminhoAtual, [Estado | CaminhoAtual],Sala) :-
    objetivo(Estado,Sala).

busca_profundidade_aux(Estado, CaminhoAtual, Caminho, Sala) :-
    expandir(Sala, Estado, [], Sucessores),
    member(Sucessor, Sucessores),
    \+ member(Sucessor, CaminhoAtual),
    busca_profundidade_aux(Sucessor, [Sucessor | CaminhoAtual], Caminho,Sala).
	
%-----------------Imprime a sala-----------------------
imprime_sala(Sala) :-
    imprime_linhas(Sala).

imprime_linhas([]).
imprime_linhas([Linha | Corpo]) :-
    imprime_elementos(Linha),
    nl,
    imprime_linhas(Corpo).

imprime_elementos([]).
imprime_elementos([Elemento | Corpo]) :-
    write(Elemento),
    write(' '),
    imprime_elementos(Corpo).

%Exemplo de entrada por enquanto : inicia_sala(Matriz, 5, 5,Sala).