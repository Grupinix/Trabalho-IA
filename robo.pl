% Sala com 48 espaços , formando uma amtrix 6x8, o é objeto, l é limpo/livre e s é sujo
sala([
    [l, s, l, l, o, s, l, o],
	[l, s, s, l, o, l, s, o],
	[s, o, l, l, s, o, o, l],
	[o, l, s, o, s, l, s, l],
	[s, o, l, s, o, s, l, o],
	[l, l, s, o, l, s, o, l]
    ]).
% Posição inicial do robô
p(0, 0).

% Movimenta o robô para cima
move_c :-
	posicao(X, Y),
	Y1 is Y - 1,
	sala(Sala),
	nth0(Y1, Sala, Linha),
	nth0(X, Linha, limpo),
	retract(posicao(X, Y)),
	assert(posicao(X, Y1)).

% Movimenta o robô para baixo
move_b :- ...
    
% Movimenta o robô para esquerda
move_e :- ...
    
% Movimenta o robô para direita
move_direita :- ...
    
% Movimenta o robô para diagonal esquerda baixo
move_eb :- ...

% Movimenta o robô para diagonal esquerda cima
move_ec :- ...

% Movimenta o robô para diagonal direita baixo
move_db :- ...

% Movimenta o robô para diagonal direita cima
move_dc :- ...
    
% Limpa a posição atual do robô
limpa :-
posicao(X, Y),
sala(Sala),
nth0(Y, Sala, Linha),
nth0(X, Linha, sujo),
retract(nth0(Y, Sala, Linha)),
assert(nth0(Y, Sala, [X=limpo | RestoLinha])).
% Ciclo principal do robô
robot :-
posicao(X, Y),
limpa,
move_cima;
move_baixo;
move_esquerda;
move_direita.