module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3 
import Desenhar

-- | Função que o estado do jogo de acordo com o tempo
reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it | estadoMenu it == Jogando, contaOndas (head (portaisJogo (jogo it))) == 4 = it {jogo = jogoInicial, estadoMenu = Ganhou}
                | estadoMenu it == Jogando, vidaBase (baseJogo (jogo it)) <= 0 = it {jogo = jogoInicial, estadoMenu = Perdeu}
                | estadoMenu it == Jogando, velocidadeJogo it == 1 = it {jogo = atualizaJogo t (jogo it)}
                | estadoMenu it == Jogando, velocidadeJogo it == 2 = it {jogo = atualizaJogo (2*t) (jogo it)}
                | estadoMenu it == Jogando2, contaOndas (head (portaisJogo (jogo it))) == 4 = it {jogo = jogoInicial, estadoMenu = Ganhou}
                | estadoMenu it == Jogando2, vidaBase (baseJogo (jogo it)) <= 0 = it {jogo = jogoInicial, estadoMenu = Perdeu}
                | estadoMenu it == Jogando2, velocidadeJogo it == 1 = it {jogo = atualizaJogo t (jogo it)}
                | estadoMenu it == Jogando2, velocidadeJogo it == 2 = it {jogo = atualizaJogo (2*t) (jogo it)}
                | otherwise = it

