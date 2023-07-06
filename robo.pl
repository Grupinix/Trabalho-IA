%Otimizar função pra printar matriz
% Inicia matriz com linhas e colunas definidas pelo usuário, as sujeiras ou obstáculos são adicionados pelo "s" e pelo "o", respectivamente.
% Eles serão adicionados em situações randômicas, enquanto os lugares limpos são representados por "l".
%custo
%Limpo, 1 custo
%Sujeira, 2 custos
%Obstáculos, 3 custos

obstaculo('o').

:- dynamic sala/1.
:- dynamic objetivoF/1.
:- dynamic objetivoP/1.

:- dynamic sujeira/1.

inicia_sala(Linhas, Colunas,Sala,Obstaculos) :-
    length(Matriz, Linhas),
    inicia_linhas(Matriz, Colunas),
    LinhaFinal is Linhas -1,
    ColunaFinal is Colunas -1,
    assertz(objetivoF([LinhaFinal,ColunaFinal])),
    
    define_obstaculos(Obstaculos,Linhas,Colunas,Matriz,SalaParcial),
    substituir_char(SalaParcial,0,0,SalaParcial1,'r'),
    substituir_char(SalaParcial1, LinhaFinal, ColunaFinal, Sala,'f'),
    assertz(sala(Sala)),
    encontrar_todas_sujeiras(Sujeiras),
    assertz(sujeira(Sujeiras)),
    write(Sala),
    atualiza_sujeira(Sujeiras).

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
    Obstaculos >= 0,
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
%-----------------------------Define objetivos------------------------

atualiza_sujeira([Cabeca|Corpo]):-
    assertz(sujeira([Cabeca|Corpo])),
    assertz(objetivoP(Cabeca)).

atualiza_sujeira([Cabeca|Corpo],Caminho) :-
    not(member(Cabeca, Caminho)),
    retractall(sujeira(_)),
    retractall(objetivoP(_)),
    assertz(sujeira([Cabeca|Corpo])),
    assertz(objetivoP(Cabeca)),
    objetivoP(X),
    write(X).

atualiza_sujeira([_|Corpo],Caminho) :-
    atualiza_sujeira(Corpo,Caminho).

atualiza_sujeira([],_) :-
    retractall(sujeira(_)),
    retractall(objetivoP(_)),
    objetivoF(X),
    assertz(objetivoP(X)).

  
% Regra para encontrar todas as sujeiras em uma posição (X, Y) na matriz
encontrar_sujeiras([X, Y]) :-
    sala(Sala),
    nth0(X, Sala, Linha), % Pega a linha Y da matriz
    nth0(Y, Linha, 's').   % Verifica se a posição X da linha é "sujo"

% Regra para encontrar todas as sujeiras na matriz
encontrar_todas_sujeiras(TodasSujeiras) :-
    findall(([X, Y]), encontrar_sujeiras([X, Y]), TodasSujeiras).

    
    
 
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

    
posicao_valida(NovoX, NovoY) :-
    sala(Sala),
    nth0(NovoX, Sala, Linha),
    nth0(NovoY, Linha, Elemento),
    objetivoF([Linhas,Colunas]),
    (NovoX =< Linhas,
    NovoX >= 0,
    NovoY =< Colunas,
    NovoY >= 0,
    not(obstaculo(Elemento))).

move_robo(Sala, [X,Y], XAntigo,YAntigo, NovaSala):- %Direita
    nth0(XAntigo, Sala, Linha),
    nth0(YAntigo, Linha, Elemento),
    Elemento = 'r',
    limpa_posicao_antiga(Sala,XAntigo,YAntigo,SalaTemporaria),
    substituir_char(SalaTemporaria,X,Y,NovaSala,'r'),
    imprime_sala(NovaSala).
move_robo(Sala, [0,0], Sala).

%----------------------------------- Algorítmos e Heurísticas----------------
expandir_profundidade(No, NovoNo) :-
    vizinho(No, NovoNo).

expandir_largura([No|Caminho], NovosSucessores) :-
    findall([NovoNo, No|Caminho], % Adiciona o novo nó no início do caminho
        (vizinho(No, NovoNo), not(member(NovoNo, [No|Caminho]))),
        NovosSucessores).


vizinho([X, Y], [NovoX, Y]) :- NovoX is X+1, posicao_valida(NovoX, Y).
vizinho([X, Y], [NovoX, Y]) :- NovoX is X-1, posicao_valida(NovoX, Y).
vizinho([X, Y], [X, NovoY]) :- NovoY is Y+1, posicao_valida(X, NovoY).
vizinho([X, Y], [X, NovoY]) :- NovoY is Y-1, posicao_valida(X, NovoY).
vizinho([X, Y], [NovoX, NovoY]) :- NovoX is X-1, NovoY is Y+1, posicao_valida(NovoX, NovoY).
vizinho([X, Y], [NovoX, NovoY]) :- NovoX is X+1, NovoY is Y-1, posicao_valida(NovoX, NovoY).

estendeCusto([_,No|Caminho],NovosCaminhos) :-
        findall([CustoNovo,NovoNo,No|Caminho],
      (
        vizinho(No,NovoNo),
        not(member(NovoNo,[No|Caminho])),
      	distancia_custo(CustoNovo,[No|Caminho])
      ),
      NovosCaminhos
       ).

estendeAvaliacao([_,No|Caminho],NovosCaminhos) :-
	findall([ValorAvaliacao,NovoNo,No|Caminho],
	(
    	vizinho(No,NovoNo),
		distancia_manhattan(NovoNo,Avaliacao),
		not(member(NovoNo,[No|Caminho])),
		ValorAvaliacao is Avaliacao),
		NovosCaminhos
	).

estendeF([_,_,_,No|Caminho],NovosCaminhos):-
    findall([FNovo,CustoNovo,AvaliacaoNovo,NovoNo,No|Caminho],
    (
    	vizinho(No,NovoNo),
        custo_avaliacao(CustoNovo,AvaliacaoNovo,Caminho,NovoNo),
        not(member(NovoNo,[No|Caminho])),
        FNovo is CustoNovo + AvaliacaoNovo),
    NovosCaminhos
   ).

maior([F1|_],[F2|_]):-F1 > F2.

particionar(_,[],[],[]).
particionar(X,[Y|Cauda],[Y|Menor],Maior):-
	maior(X,Y),!,
	particionar(X,Cauda,Menor,Maior).
particionar(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionar(X,Cauda,Menor,Maior).

ordena([],[]).
ordena([X|Cauda],ListaOrd):-
	particionar(X,Cauda,Menor,Maior),
	ordena(Menor,MenorOrd),
	ordena(Maior,MaiorOrd),
	concat(MenorOrd,[X|MaiorOrd],ListaOrd).


concat([], L, L).
concat([X | L1], L2, [X | L3]) :-
    concat(L1, L2, L3).
%---------------------------Inicio das Heuristicas----------------------
distancia_manhattan([X,Y], Distancia) :-
    objetivoP([XF,YF]),  
    Distancia is abs(X - XF) + abs(Y - YF).

%1 pois é o custo com o novo nó que entrará na lista
distancia_custo(1, []).
distancia_custo(X, [_ | Calda]) :-
    distancia_custo(Y, Calda),
    X is Y + 1.

custo_avaliacao(Custo,Avaliacao,Caminho, Estado):-
    distancia_custo(Custo,Caminho),
    distancia_manhattan(Estado,Avaliacao).

%---------------------------- Busca Profundidade--------------------------
profundidade(_, Caminho, NoCorrente, Solucao) :-
    objetivoP(NoCorrente),
    reverse(Caminho, Solucao).

profundidade(Sala, Caminho, NoCorrente, Solucao) :-
    expandir_profundidade(NoCorrente, NoNovo),
    not(member(NoNovo, Caminho)),
    profundidade(Sala, [NoNovo | Caminho], NoNovo, Solucao).

busca_profundidade(Sala, NoInicial, Solucao) :-
    profundidade(Sala, [NoInicial], NoInicial, Solucao).

    
%---------------------------- Busca Largura--------------------------
largura(_, [[No|Caminho]|_], Solucao) :-
    objetivoP(No), % Verifica se o nó atual é o objetivo
    reverse([No|Caminho], Solucao). % Inverte a solução encontrada e retorna

largura(Sala, [[No|Caminho]|CaminhosRestantes], Solucao) :-
    expandir_largura([No|Caminho], NovosCaminhos),
    append(CaminhosRestantes, NovosCaminhos, CaminhosAtualizados),
    largura(Sala, CaminhosAtualizados, Solucao).


%---------------------------- Busca Hill Climb--------------------------
hillClimb([[_,No|Caminho]|_],Solucao,'-') :-
	objetivoP(No),
	reverse([No|Caminho],Solucao).

hillClimb([Caminho|Caminhos], Solucao, Custo) :-
	estendeAvaliacao(Caminho, NovosCaminhos),
	ordena(NovosCaminhos, CaminhosOrd),
	concat(CaminhosOrd, Caminhos, CaminhosTotal),
	hillClimb(CaminhosTotal, Solucao, Custo).

%----------------------------- Busca Best-First --------------------------------

bestFirst([[_,No|Caminho]|_],Solucao, '-'):-
	objetivoP(No),
	reverse([No|Caminho],Solucao).
bestFirst([Caminho|Caminhos], Solucao, Custo):-
	estendeAvaliacao(Caminho, NovosCaminhos),
    concat(NovosCaminhos, Caminhos, CaminhosTotal),
	ordena(CaminhosTotal, CaminhosOrd),
	bestFirst(CaminhosOrd, Solucao, Custo).

%------------------ Busca Branch and Bound--------------------------------------
branchAndBound([[Custo,No|Caminho]|_],[Custo,Solucao]):-
        objetivoP(No),
        reverse([No|Caminho],Solucao).
branchAndBound([Caminho|Caminhos], Solucao):-
        estendeCusto(Caminho, NovosCaminhos),
        concat(NovosCaminhos, Caminhos, CaminhosTotal),
        ordena(CaminhosTotal,CaminhosOrd), %observar que a ordenaçao é após a concatenaçao
                                                                                     %a ordenaçao usa a mesma informaçao do Best First
        branchAndBound(CaminhosOrd, Solucao).

%------------------------- A* ---------------------------------------------------
%F = custo + avaliacao
aEstrela([[F,_,_,No|Caminho]|_],[F|Solucao]):-
    objetivoP(No),
    reverse([No|Caminho],Solucao).

aEstrela([Caminho|Caminhos], Solucao) :-
    estendeF(Caminho, NovosCaminhos),
    concat(NovosCaminhos,Caminhos,CaminhosTotal),
    ordena(CaminhosTotal,CaminhosOrd),
    aEstrela(CaminhosOrd, Solucao).

%-----------------Busca com sujeira-------------------
busca('hillClimb',Inicio):-
    objetivoP(X),
    objetivoF(Y),
    X = Y,
    concat([0],Inicio,Parametro),
    hillClimb([Parametro],Solucao,_),
    write(Solucao).

busca('hillClimb',Inicio):-
    write(Inicio),
    sujeira([_|Corpo]),
    concat([0],Inicio,Parametro),
    hillClimb([Parametro],Solucao,_),
    reverse(Solucao,CaminhoInvertido),
    atualiza_sujeira(Corpo,CaminhoInvertido),
    busca('hillClimb',CaminhoInvertido).

busca('bestFirst',Inicio):-
    objetivoP(X),
    objetivoF(Y),
    X = Y,
    concat([0],Inicio,Parametro),
    bestFirst([Parametro],Solucao,_),
    write(Solucao).

busca('bestFirst',Inicio):-
    write(Inicio),
    sujeira([_|Corpo]),
    concat([0],Inicio,Parametro),
    bestFirst([Parametro],Solucao,_),
    reverse(Solucao,CaminhoInvertido),
    atualiza_sujeira(Corpo,CaminhoInvertido),
    busca('bestFirst',CaminhoInvertido).



busca('aEstrela',Inicio):-
    objetivoP(X),
    objetivoF(Y),
    X = Y,
    aEstrela([Inicio], Solucao),
    write(Solucao).


busca('aEstrela',Inicio):-
    sujeira([_|Corpo]),
    aEstrela([Inicio], Solucao),
    Solucao = [CustoParcial | CaminhoParcial],
    reverse(CaminhoParcial,CaminhoInvertido),
    ListaParcial1 = [CustoParcial,CustoParcial,0],
    concat(ListaParcial1,CaminhoInvertido,ListaParcial2),
    atualiza_sujeira(Corpo,CaminhoInvertido),
    busca('aEstrela',ListaParcial2).

busca('aEstrela',Inicio):-
    objetivoP(X),
    objetivoF(Y),
    X = Y,
    aEstrela([Inicio], Solucao),
    write(Solucao).


    
    

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
    nl,
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


%inicia_sala(3,3,Sala,2),hillClimb([[_,[0,0]]],Solucao,Custo).
%inicia_sala(6,6,Sala,2),bestFirst([[_,[0,0]]],Solucao,Custo).
%inicia_sala(4,2,Sala,2),branchAndBound([[0,[0,0]]],Solucao).
%inicia_sala(4,4,Sala,2),aEstrela([[0,0,0,[0,0]]],Solucao).
%trace,inicia_sala(4,4,Sala,0),busca('aEstrela',[0,0,0,[0,0]]).
%trace,inicia_sala(3,3,Sala,2),busca('hillClimb',[[0,0]]).
