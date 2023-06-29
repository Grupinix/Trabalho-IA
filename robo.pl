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
    define_obstaculos(Obstaculos,Linhas,Colunas,SalaParcial,SalaParcial1),
    move_robo(Matriz, 0, 0, SalaParcial),
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
    random(1, Linhas, LinhaEscolhida),
    random(1,Colunas,ColunaEscolhida),
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
objetivo(Sala,Estado) :-
    nth0(0, Estado, X),
    nth0(1, Estado, Y),
    nth0(X, Sala, Linha),
    nth0(Y, Linha, Elemento),
    Elemento == 'f'.


expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoY is Y+1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    Sucessor = [X, NovoY].

expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoY is Y-1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    Sucessor = [X, NovoY].

expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoX is X+1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
    Sucessor = [NovoX, Y].

expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoX is X-1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
     Sucessor = [NovoX, Y].

expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
	Sucessor = [NovoX, NovoY].

expandir_profundidade(Sala, [X, Y], Sucessor) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
    Sucessor = [NovoX, NovoY].


expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoY is Y+1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    not(member([X, NovoY],[[X,Y]|Caminho])),
    append([[X, NovoY]], [[X, Y] | Caminho], NovosSucessores).

expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoY is Y-1, posicao_valida(Sala, X, NovoY, Resposta), Resposta=true),
    not(member([X, NovoY],[[X,Y]|Caminho])),
    append([[X, NovoY]], [[X, Y] | Caminho], NovosSucessores).


expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoX is X+1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
    not(member([NovoX, Y],[[X,Y]|Caminho])),
    append([[NovoX, Y]], [[X, Y] | Caminho], NovosSucessores).

expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoX is X-1, posicao_valida(Sala, NovoX, Y, Resposta), Resposta=true),
    not(member([NovoX, Y],[[X,Y]|Caminho])),
    append([[NovoX, Y]], [[X, Y] | Caminho], NovosSucessores).

expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
    not(member([NovoX, NovoY],[[X,Y]|Caminho])),
    append([[NovoX, NovoY]], [[X, Y] | Caminho], NovosSucessores).

expandir_largura(Sala, [[X, Y]|Caminho], NovosSucessores) :-
    (NovoX is X+1, NovoY is Y+1, posicao_valida(Sala, NovoX, NovoY, Resposta), Resposta=true),
    not(member([NovoX, NovoY],[[X,Y]|Caminho])),
    append([[NovoX, NovoY]], [[X, Y] | Caminho], NovosSucessores).


concat([], L, [L]).
concat([X | L1], L2, [X | L3]) :-
    concat(L1, L2, L3).
%---------------------------- Busca Profundidade--------------------------
profundidade(Sala,Caminho, NoCorrente, Solucao):-
    objetivo(Sala,NoCorrente),                          
    reverse(Caminho,Solucao).

profundidade(Sala,Caminho, NoCorrente, Solucao) :-
    expandir_profundidade(Sala,NoCorrente, NoNovo),
     not(member(NoNovo, Caminho)),
    profundidade(Sala,[NoNovo|Caminho], NoNovo, Solucao). 

busca_profundidade(Sala,NoInicial,Solucao):-
    profundidade(Sala,[NoInicial],NoInicial,Solucao).
    
%---------------------------- Busca Largura--------------------------
largura(Sala,[[[X,Y]|Caminho]|_], Solucao) :-
    objetivo(Sala,[X,Y]),
    reverse([[X,Y]|Caminho], Solucao).

largura(Sala, [[No|Caminho]|CaminhosRestantes], Solucao) :-
    findall(Caminhos,expandir_largura(Sala, [No|Caminho], Caminhos),NovosCaminhos),
    append(CaminhosRestantes, NovosCaminhos, CaminhosAtualizados),
    largura(Sala, CaminhosAtualizados, Solucao).

%---------------------------Hill Climbing-------------------------------


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
    

busca_cega(Sala):-
    write("----------Busca Cega-----------------"),
    nl,
    write("----------Profundidade---------------"),
    nl,
    busca_profundidade(Sala,[0,0],SolucaoProfundidade),
    write(SolucaoProfundidade),
    nl,
    write("----------Largura---------------"),
    largura(Sala,[[[0,0]]],SolucaoLargura),
    nl,
    write(SolucaoLargura).
   
            
    
inicio(Linhas,Colunas,Sala,Obstaculos):-
    inicia_sala(Linhas,Colunas,Sala,Obstaculos),
    imprime_sala(Sala),
    busca_cega(Sala).
    

%Exemplo de entrada por enquanto : inicia_sala(Matriz, 5, 5,Sala).