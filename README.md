# Trabalho-IA
Busca Heurística - Robô de Limpeza de Sala 

## Descrição do Problema

O problema consiste em um robô de limpeza que precisa percorrer uma matriz que representa uma sala. Cada célula da matriz pode conter uma casa suja ("s") ou uma casa limpa ("l"). O objetivo do robô é percorrer a matriz a partir do canto superior esquerdo até o canto inferior direito, realizando a limpeza das casas sujas com o menor custo possível utilizando os algoritmos de busca.

## Algoritmos de Busca

1. **Hill Climbing**:  também conhecido como busca local, é uma técnica heurística que tenta melhorar iterativamente uma solução por meio de movimentos locais. Ele começa com uma solução inicial e, em cada iteração, faz uma pequena modificação na solução atual para obter uma solução vizinha. Se a solução vizinha for melhor, ela se torna a nova solução atual e o processo é repetido. No entanto, se a solução vizinha for pior, o algoritmo pode ficar preso em um máximo local e não encontrar a melhor solução global. Portanto, o Hill Climbing é um algoritmo rápido, mas não garante a otimalidade global.

2. **A***: que se trata de um algoritmo de busca informada que combina elementos de busca em largura e busca de custo uniforme. Ele usa uma função heurística para estimar o custo do caminho restante até o objetivo. O algoritmo mantém uma lista de nós a serem explorados, ordenados por uma combinação do custo atual do caminho e da estimativa heurística do custo restante. O A* seleciona o próximo nó a ser explorado com base em uma função de avaliação que leva em consideração o custo acumulado até o nó atual e a estimativa do custo restante. Isso permite que o A* encontre uma solução ótima se a heurística for admissível, ou seja, nunca superestimar o custo real para o objetivo.
 
3. **Best First**: utiliza a função de avaliação para selecionar o próximo estado. Essa função estima a qualidade de um estado com base na distância ao objetivo. O algoritmo sempre escolhe o estado que parece ser o mais promissor em termos de se aproximar da solução, sem levar em consideração a otimalidade global. Ele pode ser eficiente em termos de tempo, mas não garante encontrar a melhor solução.

4. **Branch and Bound**: é uma técnica de busca completa que explora o espaço de solução de forma sistemática, dividindo-o em subproblemas menores. Ele usa uma estratégia de ramificação e poda para evitar explorar subárvores que não contêm soluções melhores do que a melhor solução atualmente encontrada. O algoritmo mantém um limite superior para a solução ótima atual e usa isso para determinar se uma subárvore deve ser explorada ou podada. O Branch and Bound é garantido para encontrar a melhor solução, mas pode ser computacionalmente intensivo em problemas com espaço de solução grande.


## Utilização do Sistema

1. Abrir o Swish: [Link](https://swish.swi-prolog.org/)
2. Dar um clone do código
3. Utilizar na console: ?- main(N, M, Num_de_obstaculos, HC, BB, BF, A).
4. N: Número de linhas da Matriz
5. M: Número de colunas da Matriz
6. Num_de_obstaculos: Número máximo de obstáculos que terão na Matriz
7. HC, BB, BF, A: Passar variáveis

## Resultados

Após a execução, o sistema exibirá o caminho percorrido pelo robô e o custo total obtido com o algoritmo selecionado. Você poderá comparar os resultados obtidos com cada algoritmo e analisar o desempenho em termos de custo e eficiência.

## Conclusão

O sistema implementado em Prolog oferece uma solução para o problema do robô de limpeza percorrendo uma matriz de casas sujas. Os algoritmos de busca Hill Climbing, A*, Best First e Branch and Bound fornecem diferentes abordagens para encontrar o caminho com menor custo. O usuário pode explorar cada algoritmo e avaliar seus resultados para encontrar a melhor estratégia de limpeza.

## Links
* O Relatório pode ser encontrado [aqui](https://docs.google.com/document/d/1khfaEIUcWlTwNH7YxlN2JnXubhPXSRlLOFdRBmw60Zs/edit?usp=sharing)
* A Apresentação pode ser encontrada [aqui](https://docs.google.com/presentation/d/16WAiHOZCSLWqtxThVgdTz_sN3QkA_izi93XGIGnG1JI/edit?usp=sharing) 
