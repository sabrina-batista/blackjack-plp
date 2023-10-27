:- encoding(utf8).
:- use_module(library(lists), [nth0/3]).
:- use_module(library(random), [random/3]).
:- dynamic carta/2, dinheiro/1, cartaNaMao/3, naVez/1, aposta/1, apostaTotal/1, duplicaMao/1, mao/2, numeroMaos/1, maoAtual/1, primeiraCartaDealer/1, blackjack/0.
:- retractall(dinheiro(_)), assert(dinheiro(1000)).
:- retractall(naVez(_)).
:- retractall(aposta(_)).
:- retractall(apostaTotal(_)).

main :- 
    write('------------------------------------------------------'), nl,
    write(' _      _               _       _               _'), nl,
    write('| |__  | |  __ _   ___ | | __  (_)  __ _   ___ | | __'), nl,
    write('| \'_ \\ | | / _` | / __|| |/ /  | | / _` | / __|| |/ /'), nl,
    write('| |_) || || (_| || (__ |   <   | || (_| || (__ |   <'), nl,
    write('|_.__/ |_| \\__,_| \\___||_|\\_\\ _/ | \\__,_| \\___||_|\\_\\ '), nl,
    write('                             |__/'), nl,
    write('------------------------------------------------------'), nl, jogo.

% Baralho de cartas para o jogo. 6 baralhos.
embaralharCartas :-
    retractall(carta(_,_)),
    assert(carta('A', 24)),
    assert(carta(2, 24)),
    assert(carta(3, 24)),
    assert(carta(4, 24)),
    assert(carta(5, 24)),
    assert(carta(6, 24)),
    assert(carta(7, 24)),
    assert(carta(8, 24)),
    assert(carta(9, 24)),
    assert(carta(10, 24)),
    assert(carta('J', 24)),
    assert(carta('Q', 24)),
    assert(carta('K', 24)).
    
:- retractall(carta(_,_)), embaralharCartas.

naVez('J').
aposta(0).
    
% Predicados Auxiliares

% retorna a versao string de uma carta

cartaToString(X) :-
    writeln('+---------+'),
    write('| '), write(X), writeln('       |'),
    writeln('|         |'),
    writeln('|         |'),
    writeln('|         |'),
    writeln('|         |'),
    writeln('|         |'),
    write('|       '), write(X), writeln(' |'), 
    writeln('+---------+').

% imprime na tela a mao atual do dealer
imprimirMaoDealer :-
    maoAtual(Mao),
    write('Mão do dealer: '), write(Mao), nl,
    findall(Carta, cartaNaMao('D', Carta, Mao), Cartas),
    writeln('Cartas: '), imprimirCartas(Cartas), nl.

% imprime na tela a mao atual do jogador
imprimirMaoJogador :-
    maoAtual(Mao),
    write('Mão do jogador: '), write(Mao), nl,
    findall(Carta, cartaNaMao('J', Carta, Mao), Cartas),
    writeln('Cartas: '), nl, imprimirCartas(Cartas), nl.
    
% metodo auxiliar para imprimir as cartas da mao
imprimirCartas([]).
imprimirCartas([Carta|Resto]) :-
    cartaToString(Carta),
    imprimirCartas(Resto).

somaLista([], 0).
somaLista([H|T], S) :-
    somaLista(T, S1),
    S is S1 + H.
    
% Soma as cartas de uma lista, convertendo 'A' para 11 e 'J', 'Q', 'K' para 10
somaCartas([], 0).
somaCartas([H|T], Soma) :-
    somaCartas(T, Soma1),
    (H == 'A' -> Soma is Soma1 + 11 ;
    (member(H, ['J', 'Q', 'K']) -> Soma is Soma1 + 10 ;
        (number(H) -> Soma is Soma1 + H
        )
    )).

quantidadeMaosDuplicadas(N) :-
    findall(X, (mao(X,v), duplicaMao(X)), L),
    length(L, N).

quantidadeMaosNaoDuplicadas(N) :-
    findall(X, (mao(X,v), not(duplicaMao(X))), L),
    length(L, N).
    
quantidadeMaosDuplicadasEmpate(N) :-
    findall(X, (mao(X,e), duplicaMao(X)), L),
    length(L, N).
    
quantidadeMaosNaoDuplicadasEmpate(N) :-
    findall(X, (mao(X,e), not(duplicaMao(X))), L),
    length(L, N).
    
    
temDuasCartasIguais :-
    maoAtual(M),
    findall(X, cartaNaMao('J', X, M), L),
    nth0(0, L, C1), nth0(1, L, C2),
    (C1 == C2; C1 == 'A', C2 == 1; C1 == 1, C2 == 'A').
    
    
% adiciona as cartas de um determinado jogador. Primeiro, soma todos os Ases como onze, e se o valor total exceder 21, então um ás de cada vez (muda quantas unidades forem necessárias)
valorMao(Soma, J, Mao) :-
    findall(C, cartaNaMao(J,C,Mao), L),
    somaCartas(L, Soma1),
    (Soma1 > 21 ->
    (
       cartaNaMao(J, 'A', Mao) -> (retract(cartaNaMao(J, 'A', Mao)), assert(cartaNaMao(J, 1, Mao)), !, valorMao(Soma, J, Mao)); (Soma is Soma1)
    ); Soma is Soma1).

todasAsMaosAcima21 :-
    valorMao(Soma1, 'J', 1), valorMao(Soma2, 'J', 2),
    valorMao(Soma3, 'J', 3), valorMao(Soma4, 'J', 4),
    Soma1 > 21, Soma2 > 21, Soma3 > 21, Soma4 > 21.
    
    
definirResultadoMao(Mao, Resultado) :-
    retract(mao(Mao, _)),
    assert(mao(Mao, Resultado)).
    

% muda jogador na vez
mudarJogadorNaVez :-
    retract(naVez(J)),
    (J == 'J' -> assert(naVez('D')); assert(naVez('J'))).
    
% inicia o jogo
prepararJogo :-
    retractall(cartaNaMao(_,_,_)),  % argumentos: jogador (dealer ou jogador), carta, mao. O terceiro argumento (mao) representa a mao para a qual a carta será movida assim que o par de cartas sofrer split
    retractall(duplicaMao(_)),
    retractall(mao(_,_)),
    retractall(numeroMaos(_)),
    retractall(maoAtual(_)),
    retractall(apostaTotal(_)),
    retractall(primeiraCartaDealer(_)),
    retractall(blackjack),
    assert(mao(1, d)),  % representa todas as cartas que o jogador possui (com os índices correspondentes). O segundo argumento pode ser: v (vitória - vitória naquela mao sobre o dealer), p (perda - perda naquela mao sobre o dealer), e (empate - nem vitória nem perda naquela mao sobre o dealer, número igual de cartas), d ( desconhecido - vitória ou derrota ainda não é conhecida)
    assert(maoAtual(1)),
    assert(numeroMaos(1)),
    assert(apostaTotal(0)).

% dá uma carta aleatória do baralho. Apenas as cartas que EXISTEM no baralho são levadas em consideraçao
randomCarta(Carta) :-
    random(0, 13, Rnd),
    nth0(Rnd, ['A', 2, 3, 4, 5, 6, 7, 8, 9, 10, 'J', 'Q', 'K'], C),
    carta(C, N),
    (N =:= 0 -> (!, randomCarta(Carta)) ; Carta = C).
                   
                   
% compra uma carta e tira ela do baralho
comprarCarta(Carta) :-
    randomCarta(Carta), % Escolhe uma carta aleatória do baralho
    retract(carta(Carta, N)), % Remove a carta do baralho
    N1 is N - 1, 
    assert(carta(Carta, N1)). % Atualiza o número de cartas no baralho


% total de cartas do baralho
totalCartasBaralho(N) :-
    findall(X, carta(_, X), L), !,
    somaLista(L, N).
    
    
% numero de cartas na mao do jogador
totalCartasJogador(J, N, Mao) :-
    findall(C, cartaNaMao(J, C, Mao), L),
    length(L, N).
    
% retorna verdadeiro caso o jogador possa fazer split (tem duas cartas iguais e tem menos de 4 maos)
podeSplit :-
    maoAtual(M),
    totalCartasJogador('J', 2, M),
    temDuasCartasIguais,
    numeroMaos(Total), Total < 4.


% tira dinheiro do jogador
tireDinheiro(X) :-
    retract(dinheiro(Dinheiro)), Dinheiro2 is Dinheiro - X,
    assert(dinheiro(Dinheiro2)),
    retract(apostaTotal(Total)),
    Total1 is Total + X, assert(apostaTotal(Total1)).
    
% capta os valores de aposta do jogador.
inserirAposta :-
    dinheiro(Dinheiro),
    (Dinheiro > 0 ->  % Verifica se o jogador tem dinheiro suficiente
        write('Insira um valor de aposta válido: '), read(X),
        (X =< Dinheiro ->  % Verifica se a aposta é válida
            tireDinheiro(X),
            retractall(aposta(_)),
            assert(aposta(X)),
            write('Aposta inserida com sucesso! ')
            ;  % Se a aposta for maior que o dinheiro disponível
            write('Valor inserido muito alto! Você tem '), write(Dinheiro), writeln(' reais.'),
            !, inserirAposta
        )
        ;  % Se o jogador não tem dinheiro suficiente
        write('Você ficou sem dinheiro! O jogo terminou.'), nl,
        halt  % Encerra o jogo
    ),
    !.
    
% J representa o jogador e D o dealer
distribuirCartas(D2) :-
    comprarCarta(J1), comprarCarta(D1), comprarCarta(J2), comprarCarta(D2),
    assert(cartaNaMao('J', J1, 1)), assert(cartaNaMao('D', D1, 1)), assert(cartaNaMao('J', J2, 1)), assert(cartaNaMao('D', D2, 1)),
    assert(primeiraCartaDealer(D1)),
    write('-> Nova distribuição'), nl,
    writeln('---------- Cartas ----------'),
    writeln('Dealer: '), cartaToString(D1), nl,
    writeln('+---------+'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('|░░░░░░░░░|'),
    writeln('+---------+'), nl,
    writeln('Jogador: '), cartaToString(J1), nl, cartaToString(J2), nl.

% pega uma carta do baralho e adiciona a mao do jogador
comprarCartaParaMao(Carta) :-
    naVez(J),
    comprarCarta(Carta), maoAtual(M),
    assert(cartaNaMao(J, Carta, M)).

% se o jogador obtiver blackjack imediatamente com as duas primeiras cartas. Neste caso, o casino paga numa proporção de 3:2
blackjackMaoInicial :-
    writeln('VITÓRIA! PARABÉNS!'),
    dinheiro(DinheiroJogador), aposta(ApostaJogador), NovoSaldo is DinheiroJogador + 1.5 * ApostaJogador,
    retract(dinheiro(_)), assert(dinheiro(NovoSaldo)), assert(blackjack).
    
% Se tanto o jogador como o dealer obtiverem blackjack imediatamente com as duas primeiras cartas, é um empate e o dinheiro é devolvido ao jogador
ehEmpate :-
    writeln('EMPATE!'),
    dinheiro(DinheiroJogador), aposta(ApostaJogador), NovoSaldo is DinheiroJogador + ApostaJogador,
    retract(dinheiro(_)), assert(dinheiro(NovoSaldo)), assert(blackjack).

% dá dinheiro ao jogador, dado o número de maos ganhas, o número de maos duplicadas ganhos e o número de maos com empate
realizarPagamento(Ganho) :-
    dinheiro(Dinheiro), aposta(Aposta),
    quantidadeMaosDuplicadas(N1),
    quantidadeMaosNaoDuplicadas(N2),
    quantidadeMaosDuplicadasEmpate(N3),
    quantidadeMaosNaoDuplicadasEmpate(N4),
    Ganho is 4 * N1 * Aposta + 2 * (N2 + N3) * Aposta + N4 * Aposta,
    NovoSaldo is Dinheiro + Ganho,
    retract(dinheiro(_)), assert(dinheiro(NovoSaldo)).



double(X) :-
    assert(duplicaMao(X)).

split :-
    maoAtual(M),
    findall(X, cartaNaMao('J', X, M), L),
    nth1(2, L, C),
    retract(cartaNaMao('J', C, M)),
    NovaPosicao is M + 1,
    assert(cartaNaMao('J', C, NovaPosicao)),
    assert(mao(NovaPosicao, d)),
    comprarCartaParaMao(Carta), writeln('Nova carta: '), cartaToString(Carta), write('Mao: '), writeln(M).
    
% se for a última mao do jogador, paramos o jogo e o dealer continua
proximaMao :-
    maoAtual(M), numeroMaos(M).

% se não for a última mao do jogador, aumentamos a mao atual e solicitamos a entrada do jogador para a próxima mao
proximaMao :-
    maoAtual(T), numeroMaos(Total),
    T < Total, T1 is T + 1,
    retract(maoAtual(_)), assert(maoAtual(T1)), !,
    (naVez('J') -> ( writeln('Proxima Mão'), executarAcao('J') ); acaoDoDealer ).


% quando o dealer começar a jogar, ele terá que comparar sua mao com todas as suas cartas. Portanto, antes disso é necessário definir a mao atual do jogador para 1
setPrimeiraMao :-
    retractall(maoAtual(_)),
    assert(maoAtual(1)).


% O dealer usará sempre o hit, desde que o total da sua mao seja inferior a 17.
acaoDoDealer :-
    valorMao(Soma, 'D', 1), Soma < 17,
    comprarCartaParaMao(Carta), nl, cartaToString(Carta), !,
    acaoDoDealer.

% Se o dealer passou 21
acaoDoDealer :-
    valorMao(Soma, 'D', 1), Soma > 21,
    writeln('VITÓRIA! PARABÉNS!'),
    maoAtual(M),
    definirResultadoMao(M, v), !,
    proximaMao.
    
% Se o dealer decidir "stand" e o jogador tiver passado de 21
acaoDoDealer :-
    maoAtual(M),
    valorMao(SomaMaoDealer, 'D', 1), SomaMaoDealer >= 17, SomaMaoDealer =< 21, valorMao(SomaMaoJogador, 'J', M),
    SomaMaoJogador > 21, !,
    proximaMao.
    
% Se o jogador ganhou
acaoDoDealer :-
    maoAtual(M),
    valorMao(SomaMaoDealer, 'D', 1), SomaMaoDealer >= 17, SomaMaoDealer =< 21, valorMao(SomaMaoJogador, 'J', M),
    SomaMaoJogador > SomaMaoDealer,
    writeln('VITÓRIA! PARABÉNS'),
    definirResultadoMao(M, v), !,
    proximaMao.

% Se o dealer decidir "stand" e dealer e jogador tiverem a mesma soma de cartas
acaoDoDealer :-
    maoAtual(M),
    valorMao(Soma, 'D', 1), Soma >= 17, Soma =< 21, valorMao(Soma, 'J', M),
    writeln('EMPATE!'),
    definirResultadoMao(M, e), !,
    proximaMao.

% Se o dealer decidir "stand" e o jogador perder
acaoDoDealer :-
    maoAtual(M),
    valorMao(SomaMaoDealer, 'D', 1), SomaMaoDealer >= 17, SomaMaoDealer =< 21, valorMao(SomaMaoJogador, 'J', M),
    SomaMaoDealer > SomaMaoJogador,
    writeln('DERROTA! :('),
    definirResultadoMao(M, p), !,
    proximaMao.

    

% executa a opção selecionada (hit, stand, double, split)
acaoDoJogador(s) :-
    !, proximaMao.
    
% se o jogador não tiver cartas iguais ou se já tiver dividido três vezes (ele tem um total de quatro maos), o jogador não poderá split novamente.
acaoDoJogador(p) :-
    not(podeSplit),
    writeln('Não é possível realizar split!'),
    executarAcao('J').

% se o jogador dividiu menos de três vezes (tem um total inferior a quatro maos), então a divisão é válida
acaoDoJogador(p) :-
    retract(numeroMaos(Total)), !, Total1 is Total + 1,
    assert(numeroMaos(Total1)),
    split,
    aposta(Aposta), tireDinheiro(Aposta), !,  % tiramos mais dinheiro do jogador, já que ele usou split
    executarAcao('J').
    
acaoDoJogador(h) :-
    maoAtual(M),
    numeroMaos(Total),
    comprarCartaParaMao(Carta), valorMao(Soma, 'J', M),
    write('Nova carta: '), nl, cartaToString(Carta), ( Total > 1 -> (write(', Mão: '), write(M)); true ), nl, !,
    ( Soma > 21 -> ( writeln('ESTOUROU!'), definirResultadoMao(M, p), proximaMao ); executarAcao('J') ).
    
acaoDoJogador(d) :-
    maoAtual(M),
    totalCartasJogador('J', N, M), N > 2,
    writeln('Não é possível double! Você tem mais de 2 cartas em sua mão.'),
    executarAcao('J').
    

% se o jogador tiver 2 cartas em maos, ele pode fazer double
acaoDoJogador(d) :-
    comprarCartaParaMao(Carta), maoAtual(M),
    numeroMaos(Total),
    writeln('Nova carta: '), cartaToString(Carta), ( Total > 1 -> (write(', Mão: '), write(M)); true ), nl,
    aposta(Total), tireDinheiro(Total),  % tiramos mais dinheiro do jogador, já que ele fez double
    double(M),
    valorMao(Soma, 'J', M), !,
    ( Soma > 21 -> ( writeln('ESTOUROU!'), definirResultadoMao(M, p), proximaMao ); proximaMao ).



% se for a vez do dealer e o jogador já tiver jogado blackjack (sem splits), o dealer pula o jogo
executarAcao('D') :-
    valorMao(21, 'J', 1), totalCartasJogador('J', 2, 1),
    numeroMaos(1).

% se for a vez do dealer e o jogador tiver anteriormente um mao menor que 21
executarAcao('D') :-
    maoAtual(Mao),
    valorMao(SomaMaoJogador, 'J', Mao), SomaMaoJogador =< 21,
    valorMao(SomaMaoDealer, 'D', 1),
    (SomaMaoDealer < 17 -> writeln('Novas cartas do dealer: '); true), acaoDoDealer.
    
% se for a vez do dealer e todas as maos do jogador excederem 21, então o dealer pula o jogo
executarAcao('D') :-
    todasAsMaosAcima21.

% se for a vez do dealer e o jogador tinha anteriormente um mao superior a 21, o dealer passa para a próxima mao do jogador
executarAcao('D') :-
    maoAtual(Mao),
    valorMao(SomaMaoJogador, 'J', Mao), SomaMaoJogador > 21, !,
    proximaMao.


    
    
% vez do jogador: o jogador insere (h)it / (s)tand / (d)ouble / s(p)lit, desde que a soma de suas cartas seja inferior a 21 ou até ele desistir com (s)tand
% se o jogador tiver blackjack, ele ganha automaticamente, a menos que o dealer também tenha blackjack, nesse caso é um empate
executarAcao('J') :-
    totalCartasJogador('J', 2, 1),
    valorMao(21, 'J', 1),
    numeroMaos(1),
    valorMao(N, 'D', 1),
    (N =:= 21 -> ehEmpate; blackjackMaoInicial ).
    
% Se apenas o dealer tiver blackjack, então o jogador perde
executarAcao('J') :-
    totalCartasJogador('J', 2, 1),
    valorMao(Soma, 'J', 1), Soma =\= 21,
    numeroMaos(1),
    valorMao(21, 'D', 1),
    primeiraCartaDealer('A'),
    writeln('O dealer tem blackjack!').
    
% Se o jogador tiver apenas uma carta em maos, ele recebe outra carta
executarAcao('J') :-
    maoAtual(M),
    totalCartasJogador('J', 1, M),
    acaoDoJogador(h).
    
% Se o jogador puder fazer split (e tiver dinheiro), adicionamos essa opção a ele
executarAcao('J') :-
    podeSplit,
    aposta(Total), dinheiro(N), Total =< N,
    writeln('Insira a opção: (h)it / (s)tand / (d)ouble / s(p)lit:'), read(X),
    acaoDoJogador(X).

% Se ele tiver apenas duas cartas na mao, ele também pode fazer double
executarAcao('J') :-
    maoAtual(M),
    totalCartasJogador('J', 2, M),
    aposta(Total), dinheiro(N), Total =< N,
    writeln('Insira a opção: (h)it / (s)tand / (d)ouble:'), read(X),
    acaoDoJogador(X).
    
% Ele sempre pode usar hit/stand
executarAcao('J') :-
    writeln('Insira a opção: (h)it / (s)tand:'), read(X),
    acaoDoJogador(X).


% Antes do início de cada distribuição de cartas, o número total das cartas restantes é verificado. Se a quantidade for menos que um terço do baralho, as cartas são embaralhadas. Todas as cartas também são removidas das mãos do jogador e do dealer.
jogo :-
    prepararJogo,
    totalCartasBaralho(N), (N =< 104 -> embaralharCartas; true),
    inserirAposta,
    distribuirCartas(Oculta),
    executarAcao('J'),
    imprimirMaoJogador,
    mudarJogadorNaVez,
    setPrimeiraMao,
    write('A carta oculta do dealer: '), nl, cartaToString(Oculta), nl,
    executarAcao('D'),
    imprimirMaoDealer,
    mudarJogadorNaVez,
    realizarPagamento(Ganho), apostaTotal(Aposta),
    ( not(blackjack) -> ( Ganho >= Aposta -> writeln('Ganhou mais ou igual ao que apostou'); writeln('Perdeu mais do que apostou') ); true ),
    dinheiro(Dinheiro), write('Saldo: '), write(Dinheiro), nl, nl,
    writeln('Novo jogo'),
    !, jogo.