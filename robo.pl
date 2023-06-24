% Inicia matriz com linhas e colunas definidas pelo usuário, as sujeiras ou obstáculos são adicionados pelo "s" e pelo "o", respectivamente.
% Eles serão adicionados em situações randômicas, enquanto os lugares limpos são representados por "l".
inicia_sala(Matriz, Linhas, Colunas,Sala) :-
    length(Matriz, Linhas),
    inicia_linhas(Matriz, Colunas),
    move_robo(Matriz, 0, 0, Sala),
    imprime_sala(Sala). 

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

%----------------------------Robô--------------------------------------
% Movimento do robô
move_robo(Sala, X, Y, NovaSala) :-
    nth0(X, Sala, Linha), %Quero pegar a linha na posição X da Sala 
    replace(Y, Linha, 'r', NovaLinha), %Faco o replace apenas nessa linha, na posicao Y pelo r, que é o robo
    replace(X, Sala, NovaLinha, NovaSala). %Aplico o replace novamente na matriz, trocando a posição das linhas

replace(Indice, Lista, Elemento, NovaLista) :-
    nth0(Indice, Lista, _, Temporaria),
    nth0(Indice, NovaLista, Elemento, Temporaria).

limpa_posicao_antiga(Sala, XAntigo, YAntigo, NovaSala):-
    nth0(XAntigo, Sala, Linha), 
    replace(YAntigo, Linha, 'l', NovaLinha), 
    replace(XAntigo, Sala, NovaLinha, NovaSala). 

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

d(Sala, XAtual, YAtual, NovaSala):- %Diagonal 
    limpa_posicao_antiga(Sala,XAtual,YAtual,SalaTemporaria),
    NovoX is XAtual+1,
    NovoY is YAtual+1,
    move_robo(SalaTemporaria, NovoX, NovoY, NovaSala).


%----------------------------------- Algorítmos e Heurísticas----------------
%TODO
%------------------------------------Imprime a sala-----------------------
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