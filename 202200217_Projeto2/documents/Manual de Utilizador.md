# Manual de Utilizador do **Projeto Nº2**: Época de Recurso
Inteligência Artificial - Escola Superior de Tecnologia de Setúbal
2024/2025  
Estudante: **Rodrigo Baptista**, número **202200217**  

## 1. Introdução
O jogo Adji-boto é um jogo de estratégia baseado no clássico Mancala, onde o objetivo principal é capturar o maior número de peças possível.  
Existem 2 jogadores que competem entre si e cada jogador controla uma linha do tabuleiro. Em cada turno, o jogador escolhe um dos seus buracos para distribuir as peças no sentido contrário aos ponteiros de um relógio. Se ao final do movimento a última peça cair num buraco do adversário (linha oposta à inicial) e houver 1, 3 ou 5 peças nesse buraco, essas peças podem ser capturadas.  
O jogo termina quando não há mais peças no tabuleiro, e o vencedor é aquele que capturou mais peças.  
Pode ser jogado em modo Humano vs Computador ou Computador vs Computador para simular as melhores jogadas possíveis para cada situação.
## 2. Como Jogar
1. Fazer download dos três ficheiros e colocá-los na mesma pasta: **jogo.lisp**, **puzzle.lisp**, **algoritmo.lisp**.  
2. Abrir o LispWorks e compilar o ficheiro jogo.lisp, que irá carregar automaticamente os outros dois.  
3. Abrir uma nova janela do Listener e escrever o comando ```(initialize)```.  
4. Será perguntado o modo de jogo, se for Humano VS Computador o utilizador terá que inserir o primeiro jogador e o tempo de pensamento do computador, se for Computador VS Computador o utilizador apenas terá de inserir o tempo de pensamento do computador.  
5. Agora, pode jogar segundo as regras descritas anteriormente.  

Exemplo de Humano VS Computador:
```
CL-USER 1 > (initialize)

What mode? 
1 - Human VS Computer 
2 - Computer VS Computer 
Mode: 1

Who starts the game? 
1 - Human 
2 - Computer 
First Player: 1

Maximum number of seconds allowed for computer move [1, 20]: 5

----- TURN 1 -----
Board:
2 - (8 8 8 8 8 8)
1 - (8 8 8 8 8 8)
Choose a hole to move [1, 6]: 
5
Player 1 moved pieces in hole 5.
Board:
2 - (9 9 9 9 9 9)
1 - (9 8 8 8 0 9)
Score:
- Player 1 score is: 0
- Player 2 score is: 0
Player 1 and Player 2 are tied!
```
Exemplo de Computador VS Computador:
```
CL-USER 1 > (initialize)

What mode? 
1 - Human VS Computer 
2 - Computer VS Computer 
Mode: 2

Maximum number of seconds allowed for computer move [1, 20]: 5

----- TURN 1 -----
Board:
2 - (8 8 8 8 8 8)
1 - (8 8 8 8 8 8)
Computer thinking...
Number of alpha cuts: 2951
Number of beta cuts: 2782
Number of nodes analyzed: 10749
Time taken: 5004ms
Player 1 moved pieces in hole 6.
Board:
2 - (9 9 9 9 9 9)
1 - (9 9 8 8 8 0)
Score:
- Player 1 score is: 0
- Player 2 score is: 0
Player 1 and Player 2 are tied!

----- TURN 2 -----
Board:
2 - (9 9 9 9 9 9)
1 - (9 9 8 8 8 0)
Computer thinking...
Number of alpha cuts: 3075
Number of beta cuts: 2763
Number of nodes analyzed: 11399
Time taken: 5002ms
Player 2 moved pieces in hole 4.
Board:
2 - (10 10 10 0 9 9)
1 - (10 10 9 9 9 0)
Score:
- Player 1 score is: 0
- Player 2 score is: 1
Player 1 is losing by 1 points.
```
Nota: É crucial que, sempre que se inicie uma nova sessão de jogo, abra uma nova janela do Listener e volte a correr o ```(initialize)```.  
## 3. Informação Produzida
No mesmo diretório em que colocou os três ficheiros do jogo, irá ser produzido um ficheiro denominado log.dat, este ficheiro mantém um registo de todas os jogos efetuados no sistema que está a correr o programa e oferece uma análise estatística de todos os elementos presentes em cada jogada e no final de um jogo. Desta forma, é possível revisitar jogos anteriores e verificar o comportamento da IA.  
## 4. Limitações
Não existem limitações de natureza do utilizador no programa.   