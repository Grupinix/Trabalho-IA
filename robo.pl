%Otimizar função pra printar matriz
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
   
inicia_sala(Linhas, Colunas,Sala,Obstaculos) :-
    length(Matriz, Linhas),
    inicia_linhas(Matriz, Colunas),
    LinhaFinal is Linhas -1,
    ColunaFinal is Colunas -1,
    define_obstaculos(Obstaculos,Linhas,Colunas,Matriz,SalaParcial),
    substituir_char(SalaParcial,0,0,SalaParcial1,'r'),
    substituir_char(SalaParcial1, LinhaFinal, ColunaFinal, Sala,'f').

inicia_linhas([], _).
inicia_linhas([Linha | Corpo], Colunas) :-
    length(Linha, Colunas),
    preenche_sala(Linha),
    inicia_linhas(Corpo, Colunas).

define_valor(Resultado) :-
    random(0, 2, Escolha),
    (Escolha = 0 -> Resultado = 'l' ;
     Escolha = 1 -> Resultado = 's').

define_obstaculos(0, _, _, SalaParcial, SalaParcial).

define_obstaculos(Obstaculos,Linhas,Colunas,SalaParcial,Sala):-
    Obstaculos > 0,
    random(0, Linhas, LinhaEscolhida),
    random(0,Colunas,ColunaEscolhida),
	substituir_char(SalaParcial, LinhaEscolhida, ColunaEscolhida, SalaParcial1,'o'),
    NovoObstaculo is Obstaculos -1,
    define_obstaculos(NovoObstaculo,Linhas,Colunas,SalaParcial1,Sala).

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
limpa_posicao_antiga(Sala, XAntigo, YAntigo, NovaSala):-
    substituir_char(Sala,XAntigo,YAntigo,NovaSala,'l').

replace(Indice, Lista, Elemento, NovaLista) :-
    nth0(Indice, Lista, _, Temporaria),
    nth0(Indice, NovaLista, Elemento, Temporaria).

    
posicao_valida(Sala, NovoX, NovoY) :-
    length(Sala, Linhas),
    nth0(NovoX, Sala, Linha),
    nth0(NovoY, Linha, Elemento),
    length(Linha, Colunas),
    ( not(NovoX >= Linhas),
    not(NovoX < 0),
    not(NovoY >= Colunas),
    not(NovoY < 0),
    not(obstaculo(Elemento))).

move_robo(Sala, [X,Y], NovaSala):- %Direita
    vizinho(Sala,[X,Y],[XAntigo,YAntigo]),
    nth0(XAntigo, Sala, Linha),
    nth0(YAntigo, Linha, Elemento),
    Elemento = 'r',
    limpa_posicao_antiga(Sala,XAntigo,YAntigo,SalaTemporaria),
    substituir_char(SalaTemporaria,X,Y,NovaSala,'r'),
    imprime_sala(NovaSala).
move_robo(Sala, [0,0], Sala).

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
objetivo(Sala,Estado) :-
    nth0(0, Estado, X),
    nth0(1, Estado, Y),
    nth0(X, Sala, Linha),
    nth0(Y, Linha, Elemento),
    Elemento == 'f'.


expandir_profundidade(Sala, No, NovoNo) :-
    vizinho(Sala, No, NovoNo).

expandir_largura(Sala, [No|Caminho], NovosSucessores) :-
    findall([NovoNo, No|Caminho], % Adiciona o novo nó no início do caminho
        (vizinho(Sala, No, NovoNo), not(member(NovoNo, [No|Caminho]))),
        NovosSucessores).


vizinho(Sala, [X, Y], [NovoX, Y]) :- NovoX is X+1, posicao_valida(Sala, NovoX, Y).
vizinho(Sala, [X, Y], [NovoX, Y]) :- NovoX is X-1, posicao_valida(Sala, NovoX, Y).
vizinho(Sala, [X, Y], [X, NovoY]) :- NovoY is Y+1, posicao_valida(Sala, X, NovoY).
vizinho(Sala, [X, Y], [X, NovoY]) :- NovoY is Y-1, posicao_valida(Sala, X, NovoY).
vizinho(Sala, [X, Y], [NovoX, NovoY]) :- NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY).
vizinho(Sala, [X, Y], [NovoX, NovoY]) :- NovoX is X-1, NovoY is Y-1, posicao_valida(Sala, NovoX, NovoY).



concat([], L, [L]).
concat([X | L1], L2, [X | L3]) :-
    concat(L1, L2, L3).
%---------------------------Inicio das Heuristicas----------------------
distancia_manhattan(Sala,[X,Y], Distancia) :-
    objetivo(Sala,[XF,YF]),  
    Distancia is abs(X - XF) + abs(Y - YF).

%---------------------------- Busca Profundidade--------------------------
profundidade(Sala, Caminho, NoCorrente, Solucao) :-
    objetivo(Sala, NoCorrente),
    reverse(Caminho, Solucao).

profundidade(Sala, Caminho, NoCorrente, Solucao) :-
    expandir_profundidade(Sala, NoCorrente, NoNovo),
    not(member(NoNovo, Caminho)),
    profundidade(Sala, [NoNovo | Caminho], NoNovo, Solucao).

busca_profundidade(Sala, NoInicial, Solucao) :-
    profundidade(Sala, [NoInicial], NoInicial, Solucao).

    
%---------------------------- Busca Largura--------------------------
largura(Sala, [[No|Caminho]|_], Solucao) :-
    objetivo(Sala,No), % Verifica se o nó atual é o objetivo
    reverse([No|Caminho], Solucao). % Inverte a solução encontrada e retorna

largura(Sala, [[No|Caminho]|CaminhosRestantes], Solucao) :-
    expandir_largura(Sala, [No|Caminho], NovosCaminhos),
    append(CaminhosRestantes, NovosCaminhos, CaminhosAtualizados),
    largura(Sala, CaminhosAtualizados, Solucao).


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
    

busca(Sala,'largura'):-
    write("----------Busca Cega-----------------"),
    nl,
    write("----------Largura---------------"),
    largura(Sala,[[[0,0]]],SolucaoLargura),
    imprime_solucao_sala(Sala,SolucaoLargura),
    nl,
    write(SolucaoLargura).
busca(Sala,'profundidade'):-
    
    write("----------Busca Cega-----------------"),
    nl,
    write("----------Profundidade---------------"),
    nl,
    busca_profundidade(Sala,[0,0],SolucaoProfundidade),
    write(SolucaoProfundidade),
    nl.

imprime_solucao_sala(_,[]).
imprime_solucao_sala(Sala,[[X,Y]|Solucao]):-
    move_robo(Sala,[X,Y],NovaSala),
    imprime_solucao_sala(NovaSala,Solucao).

inicio(Linhas,Colunas,Sala,Obstaculos,Busca):-
    inicia_sala(Linhas,Colunas,Sala,Obstaculos),
    imprime_sala(Sala),
    busca(Sala,Busca).
    

%Exemplo de entrada por enquanto : inicio(2,2,Sala,0,'profundidade').
