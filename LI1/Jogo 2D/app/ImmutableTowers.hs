module ImmutableTowers where
import LI12425
import Graphics.Gloss



data ImmutableTowers = ImmutableTowers {
    estadoMenu :: Menu,
    jogo :: Jogo,
    imagens :: Imagens,
    novaTorre :: Maybe Torre,
    velocidadeJogo :: Int
                                       }

type Imagens = [(String, Picture)]

data Menu = Jogando
            | Jogando2
            | MenuInicial   
            | MenuPrincipal 
            | SelecionarNivel
            | MenuPausa 
            | Perdeu
            | Ganhou 
            | MenuAjuda 
            | MenuCreditos 
            | MenuLoja 
            | MenuSair 
            | ColocarTorre
            | VenderTorre
            deriving (Eq, Show)



-- | Mapa do nivel 1
nivel01 :: Mapa
nivel01 = [
  [r,r,r,r,r,r,r,r,r,r,r,r,a,r,r],
  [r,r,t,t,t,t,t,t,t,r,r,r,a,r,r],
  [r,r,t,r,r,r,r,r,t,r,r,r,p1,r,r],
  [r,r,t,r,r,r,r,r,t,r,r,r,a,r,r],
  [a,a,t,a,p2,a,a,a,t,a,a,a,a,a,a],
  [r,r,t,r,r,r,r,r,t,r,r,r,a,r,r],
  [r,r,t,r,r,r,t,t,t,r,r,r,a,r,r],
  [r,r,t,r,r,r,t,r,r,r,r,r,p1,r,r],
  [r,r,t,r,r,r,t,r,r,r,r,r,a,r,r],
  [r,r,t,r,r,r,t,r,r,r,r,r,a,r,r],
  [r,r,t,r,r,r,t,r,r,r,r,r,p1,r,r],
  [r,r,t,r,r,r,t,r,r,r,r,r,a,r,r],
  [r,r,t,r,r,r,r,r,r,r,r,r,a,r,r],
  [r,r,t,r,a,a,a,a,p2,a,a,a,a,a,a],
  [r,r,t,r,a,r,r,r,r,r,r,r,p1,r,r]]
  where r = Relva
        t = Terra
        a = Agua
        p1 = Ponte1
        p2 = Ponte2

-- | Jogo do nivel 1
jogoInicial :: Jogo
jogoInicial = Jogo {
    baseJogo = Base {vidaBase = 100, posicaoBase = (8,1), creditosBase = 350},
    portaisJogo = portais01,
    torresJogo = torres01,
    mapaJogo = nivel01,
    inimigosJogo = [],
    lojaJogo = loja01
}

-- | torres iniciais
torres01 :: [Torre]
torres01 = []

-- | Torre de fogo
torreFogo :: Torre
torreFogo = Torre {
        posicaoTorre = (3,5),
        danoTorre = 10,
        alcanceTorre = 5,
        rajadaTorre = 2,
        cicloTorre = 2,
        tempoTorre = 1,
        projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5}
}
-- | Torre de gelo
torreGelo :: Torre
torreGelo = Torre {
        posicaoTorre = (1,7),
        danoTorre = 5,
        alcanceTorre = 2,
        rajadaTorre = 1,
        cicloTorre = 8,
        tempoTorre = 2,
        projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 3}
}
-- | Torre de resina
torreResina :: Torre
torreResina = Torre {
        posicaoTorre = (3,3),
        danoTorre = 5,
        alcanceTorre = 8,
        rajadaTorre = 2,
        cicloTorre = 6,
        tempoTorre = 1.5,
        projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}
}

-- | Loja do jogo
loja01 :: Loja
loja01 = [(100,torreFogo),(250,torreGelo),(200,torreResina)]

-- | Portais do nivel 1
portais01 :: [Portal]
portais01 = [portal1,portal2]

-- | Portal 1
portal1 :: Portal
portal1 = Portal {posicaoPortal= (2,14), ondasPortal=[onda1,onda2,onda3,onda4]}

-- | Portal 2
portal2 :: Portal
portal2 = Portal {posicaoPortal= (6,12), ondasPortal=[onda1p2,onda2p2,onda3p2,onda4p2]}

-- | Primeira onda do portal 1
onda1 :: Onda
onda1 = Onda {
        inimigosOnda = [i1,i2,i1,i1,i1,i1,i1,i1,i2,i2,i2,i2,i1,i2,i2,i1,i2,i2,i2,i1,i2],
        cicloOnda = 3.5,
        tempoOnda = 2,
        entradaOnda = 2
}       where i1 = inimigo1
              i2 = inimigo2

-- | Segunda onda do portal 1
onda2 :: Onda
onda2 = Onda {
        inimigosOnda = [i1,i1,i1,i1,i1,i1,i1,i1,i2,i2,i2,i2,i1,i2,i2,i1,i2,i2,i2,i1,i2,i1,i1,i1,i1,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i2,i2,i1,i2,i2,i1,i3,i3,i3,i3,i3,i3,i2,i3,i3,i3,i3,i3,i3,i2,i2,i2,i1,i2],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 30
}       where i1 = inimigo1
              i2 = inimigo2
              i3 = inimigo3

-- | Terceira onda do portal 1
onda3 :: Onda
onda3 = Onda {
        inimigosOnda = [i1,i1,i1,i1,i1,i1,i1,i1,i2,i2,i2,i2,i1,i2,i2,i1,i2,i2,i2,i1,i2,i1,i1,i1,i1,i1,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i3,i3,i2,i3,i2,i1,i1,i1,i1,i2,i3,i1,i1,i1,i1,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i1,i1,i1,i1,i1,i1,i1,i1,i1,i1,i1,i1,i3],
        cicloOnda = 1.5,
        tempoOnda = 2,
        entradaOnda = 65
        }
        where i1 = inimigo1
              i2 = inimigo2
              i3 = inimigo3

-- | Quarta onda do portal 1
onda4 :: Onda
onda4 = Onda {
        inimigosOnda = [i3,i3,i2,i3,i2,i3,i2,i3,i2,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i3,i2,i3,i3,i2,i3,i2,i3,i2,i3,i2,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i1,i1,i1,i1,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i3,i2,i3,i2,i3,i3,i2,i3,i1,i1,i1,i1,i1,i1,i1,i1,i3,i3,i3,i2,i3,i2,i3,i2,i3,i2,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i3,i2,i3,i3,i2,i3,i2,i3,i2,i3,i2,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i1,i1,i1,i1,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i3,i2,i3,i2,i3,i2,i3,i3,i2,i3,i1,i1,i1,i1,i1,i1,i1,i1,i3],
        cicloOnda = 1,
        tempoOnda = 2,
        entradaOnda = 100
        }
        where i1 = inimigo1
              i2 = inimigo2
              i3 = inimigo3

-- | Primeira onda do portal 2
onda1p2 :: Onda
onda1p2 = Onda {
        inimigosOnda = [i4,i5,i4,i4,i4,i4,i4,i4,i5,i5,i5,i5,i4,i5,i5,i4,i5,i5,i5,i4,i5],
        cicloOnda = 3.5,
        tempoOnda = 2,
        entradaOnda = 2
}       where i4 = inimigo4
              i5 = inimigo5

-- | Segunda onda do portal 2
onda2p2 :: Onda
onda2p2 = Onda {
        inimigosOnda = [i4,i4,i4,i4,i4,i4,i4,i4,i5,i5,i5,i5,i4,i5,i5,i4,i5,i5,i5,i4,i5,i4,i4,i4,i4,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i5,i5,i5,i4,i5,i5,i4,i6,i6,i6,i6,i6,i6,i5,i6,i6,i6,i6,i6,i6,i5,i5,i5,i4,i5],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 30
}       where i4 = inimigo4
              i5 = inimigo5
              i6 = inimigo6

-- | Terceira onda do portal 2
onda3p2 :: Onda
onda3p2 = Onda {
        inimigosOnda = [i4,i4,i4,i4,i4,i4,i4,i4,i5,i5,i5,i5,i4,i5,i5,i4,i5,i5,i5,i4,i5,i4,i4,i4,i4,i4,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i5,i6,i6,i5,i6,i5,i4,i4,i4,i4,i5,i6,i4,i4,i4,i4,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i6,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i6],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 65
        }
        where i4 = inimigo4
              i5 = inimigo5
              i6 = inimigo6
              
-- | Quarta onda do portal 2
onda4p2 :: Onda
onda4p2 = Onda {
        inimigosOnda = [i4,i4,i5,i4,i5,i4,i5,i4,i5,i4,i4,i4,i4,i4,i4,i4,i4,i4,i5,i4,i5,i4,i4,i5,i4,i5,i4,i5,i4,i5,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i6,i6,i6,i6,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i5,i4,i5,i4,i5,i4,i4,i5,i4,i6,i6,i6,i6,i6,i6,i6,i6,i4,i4,i4,i5,i4,i5,i4,i5,i4,i5,i4,i4,i4,i4,i4,i4,i4,i5,i4,i5,i4,i4,i5,i4,i5,i4,i5,i4,i5,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i6,i6,i6,i6,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i4,i5,i4,i5,i4,i5,i4,i4,i5,i4,i6,i6,i6,i6,i6,i6,i6,i6,i4],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 100
        }
        where i4 = inimigo4
              i5 = inimigo5
              i6 = inimigo6

-- | Inimigo azul do primeiro portal
inimigo1 :: Inimigo
inimigo1 = Inimigo {posicaoInimigo = (2,13), 
                    direcaoInimigo = Este,
                    vidaInimigo = 150,
                    velocidadeInimigo = 1.5, 
                    ataqueInimigo = 5, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo roxo do primeiro portal
inimigo2 :: Inimigo
inimigo2 = Inimigo {posicaoInimigo = (2,13), 
                    direcaoInimigo = Norte,
                    vidaInimigo = 300,
                    velocidadeInimigo = 1, 
                    ataqueInimigo = 10, 
                    butimInimigo = 10, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }
        
-- | Inimigo verde do primeiro portal
inimigo3 :: Inimigo
inimigo3 = Inimigo {posicaoInimigo = (2,13), 
                    direcaoInimigo = Norte,
                    vidaInimigo = 500,
                    velocidadeInimigo = 0.8, 
                    ataqueInimigo = 15, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo azul do segundo portal
inimigo4 :: Inimigo
inimigo4 = Inimigo {posicaoInimigo = (6,11), 
                    direcaoInimigo = Oeste,
                    vidaInimigo = 150,
                    velocidadeInimigo = 1.5, 
                    ataqueInimigo = 5, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo roxo do segundo portal
inimigo5 :: Inimigo
inimigo5 = Inimigo {posicaoInimigo = (6,11), 
                    direcaoInimigo = Este,
                    vidaInimigo = 300,
                    velocidadeInimigo = 1, 
                    ataqueInimigo = 10, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo verde do segundo portal
inimigo6 :: Inimigo
inimigo6 = Inimigo {posicaoInimigo = (6,11), 
                    direcaoInimigo = Este,
                    vidaInimigo = 500,
                    velocidadeInimigo = 0.8, 
                    ataqueInimigo = 15, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo azul do terceiro portal
inimigo7 :: Inimigo
inimigo7 = Inimigo {posicaoInimigo = (3,12), 
                    direcaoInimigo = Oeste,
                    vidaInimigo = 150,
                    velocidadeInimigo = 1.5, 
                    ataqueInimigo = 5, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo roxo do terceiro portal
inimigo8 :: Inimigo
inimigo8 = Inimigo {posicaoInimigo = (3,12), 
                    direcaoInimigo = Este,
                    vidaInimigo = 300,
                    velocidadeInimigo = 1, 
                    ataqueInimigo = 10, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo verde do terceiro portal
inimigo9 :: Inimigo
inimigo9 = Inimigo {posicaoInimigo = (3,12), 
                    direcaoInimigo = Este,
                    vidaInimigo = 500,
                    velocidadeInimigo = 0.8, 
                    ataqueInimigo = 15, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo azul do quarto portal
inimigo10 :: Inimigo
inimigo10 = Inimigo {posicaoInimigo = (6,13), 
                    direcaoInimigo = Oeste,
                    vidaInimigo = 150,
                    velocidadeInimigo = 1.5, 
                    ataqueInimigo = 5, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo roxo do quarto portal
inimigo11 :: Inimigo
inimigo11 = Inimigo {posicaoInimigo = (6,13), 
                    direcaoInimigo = Este,
                    vidaInimigo = 600,
                    velocidadeInimigo = 1, 
                    ataqueInimigo = 10, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Inimigo verde do quarto portal
inimigo12 :: Inimigo
inimigo12 = Inimigo {posicaoInimigo = (6,13), 
                    direcaoInimigo = Este,
                    vidaInimigo = 700,
                    velocidadeInimigo = 0.8, 
                    ataqueInimigo = 15, 
                    butimInimigo = 5, -- se mexer mudar na linha 221 da Desenhar.hs
                    projeteisInimigo = []
                    }

-- | Jogo do nivel 2
jogo02 :: Jogo
jogo02 = Jogo {
    baseJogo = Base {vidaBase = 100, posicaoBase = (9,1), creditosBase = 200},
    portaisJogo = portais02,
    torresJogo = torres02,
    mapaJogo = mapa02,
    inimigosJogo = [],
    lojaJogo = loja01
}
-- | Mapa do nivel 2
mapa02 :: Mapa
mapa02 = [
        [r,r,r,r,r,r,r,r,r,r,r,r,a,r,r],
        [r,a,a,a,a,a,a,r,r,t,r,r,a,r,r],
        [r,a,a,a,a,a,a,r,r,t,r,r,p1,r,r],
        [r,a,a,a,a,a,a,r,r,t,r,r,a,r,r],
        [r,a,a,a,a,a,a,a,a,t,a,a,a,a,a],
        [r,a,a,a,a,a,a,r,r,t,r,r,a,r,r],
        [r,a,a,a,a,a,a,r,r,t,r,r,a,r,r],
        [r,r,r,r,r,r,r,r,r,t,r,r,a,r,r],
        [r,r,r,t,t,t,t,t,t,t,r,r,a,r,r],
        [r,r,r,t,r,r,t,r,r,r,r,r,a,r,r],
        [r,r,r,t,r,r,t,r,r,r,r,a,a,a,r],
        [r,r,r,t,r,r,t,r,r,r,r,a,a,a,a],
        [r,r,r,t,r,r,t,r,r,r,r,a,a,a,r],
        [r,r,r,t,r,r,t,r,r,r,r,a,r,r,r],
        [r,r,r,r,r,r,t,r,r,r,r,a,r,r,r]
        ]
        where   r = Relva
                t = Terra
                a = Agua
                p1 = Ponte1
                p2 = Ponte2
-- | torres iniciais do nivel 2
torres02 :: [Torre]
torres02 = []

-- | Portais do nivel 2
portais02 :: [Portal]
portais02 = [portal3,portal4]

-- | Portal 3
portal3 :: Portal
portal3 = Portal {posicaoPortal= (3,13), ondasPortal=[onda1p3,onda2p3,onda3p3,onda4p3]}

-- | Portal 4
portal4 :: Portal
portal4 = Portal {posicaoPortal= (6,14), ondasPortal=[onda1p4,onda2p4,onda3p4,onda4p4]}

-- | Primeira onda do portal 3
onda1p3 :: Onda
onda1p3 = Onda {
        inimigosOnda = [i7,i8,i7,i7,i7,i7,i7,i7,i8,i8,i8,i8,i7,i8,i8,i7,i8,i8,i8,i7,i8],
        cicloOnda = 3.5,
        tempoOnda = 2,
        entradaOnda = 5
}       where i7 = inimigo7
              i8 = inimigo8

-- | Segunda onda do portal 3
onda2p3 :: Onda
onda2p3 = Onda {
        inimigosOnda = [i7,i7,i7,i7,i7,i7,i7,i7,i8,i8,i8,i8,i7,i8,i8,i7,i8,i8,i8,i7,i8,i7,i7,i7,i7,i9,i9,i9,i9,i9,i9,i9,i9,i8,i8,i8,i8,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i8,i8,i8,i8,i8,i8,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 30
}       where i7 = inimigo7
              i8 = inimigo8
              i9 = inimigo9

-- | Terceira onda do portal 3
onda3p3 :: Onda
onda3p3 = Onda {
        inimigosOnda = [i7,i7,i7,i7,i7,i7,i7,i7,i8,i8,i8,i8,i7,i8,i8,i7,i8,i8,i8,i7,i8,i7,i7,i7,i7,i7,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i8,i9,i9,i8,i9,i8,i7,i7,i7,i7,i8,i9,i7,i7,i7,i7,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i9,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i9],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 65
        }
        where i7 = inimigo7
              i8 = inimigo8
              i9 = inimigo9

-- | Quarta onda do portal 3
onda4p3 :: Onda
onda4p3 = Onda {
        inimigosOnda = [i7,i7,i8,i7,i8,i7,i8,i7,i8,i7,i7,i7,i7,i7,i7,i7,i7,i7,i8,i7,i8,i7,i7,i8,i7,i8,i7,i8,i7,i8,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i9,i9,i9,i9,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i8,i7,i8,i7,i8,i7,i7,i8,i7,i9,i9,i9,i9,i9,i9,i9,i9,i7,i7,i7,i8,i7,i8,i7,i8,i7,i8,i7,i7,i7,i7,i7,i7,i7,i8,i7,i8,i7,i7,i8,i7,i8,i7,i8,i7,i8,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i9,i9,i9,i9,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i7,i8,i7,i8,i7,i8,i7,i7,i8,i7,i9,i9,i9,i9,i9,i9,i9,i9,i7],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 100
        }
        where i7 = inimigo7
              i8 = inimigo8
              i9 = inimigo9

-- | Primeira onda do portal 4
onda1p4 :: Onda
onda1p4 = Onda {
        inimigosOnda = [i10,i11,i10,i10,i10,i10,i10,i10,i11,i11,i11,i11,i10,i11,i11,i10,i11,i11,i11,i10,i11],
        cicloOnda = 3.5,
        tempoOnda = 2,
        entradaOnda = 5
}       where i10 = inimigo10
              i11 = inimigo11

-- | Segunda onda do portal 4
onda2p4 :: Onda
onda2p4 = Onda {
        inimigosOnda = [i10,i10,i10,i10,i10,i10,i10,i10,i11,i11,i11,i11,i10,i11,i11,i10,i11,i11,i11,i10,i11,i10,i10,i10,i10,i12,i12,i12,i12,i12,i12,i12,i12,i11,i11,i11,i11,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i11,i11,i11,i11,i11,i11,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 30
}       where i10 = inimigo10
              i11 = inimigo11
              i12 = inimigo12

-- | Terceira onda do portal 4
onda3p4 :: Onda
onda3p4 = Onda {
        inimigosOnda = [i10,i10,i10,i10,i10,i10,i10,i10,i11,i11,i11,i11,i10,i11,i11,i10,i11,i11,i11,i10,i11,i10,i10,i10,i10,i10,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i11,i12,i12,i11,i12,i11,i10,i10,i10,i10,i11,i12,i10,i10,i10,i10,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i12,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i12],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 65
        }
        where i10 = inimigo10
              i11 = inimigo11
              i12 = inimigo12

-- | Quarta onda do portal 4
onda4p4 :: Onda
onda4p4 = Onda {
        inimigosOnda = [i10,i10,i11,i10,i11,i10,i11,i10,i11,i10,i10,i10,i10,i10,i10,i10,i10,i10,i11,i10,i11,i10,i10,i11,i10,i11,i10,i11,i10,i11,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i12,i12,i12,i12,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i11,i10,i11,i10,i11,i10,i10,i11,i10,i12,i12,i12,i12,i12,i12,i12,i12,i10,i10,i10,i11,i10,i11,i10,i11,i10,i11,i10,i10,i10,i10,i10,i10,i10,i11,i10,i11,i10,i10,i11,i10,i11,i10,i11,i10,i11,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i12,i12,i12,i12,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i10,i11,i10,i11,i10,i11,i10,i10,i11,i10,i12,i12,i12,i12,i12,i12,i12,i12,i10],
        cicloOnda = 3,
        tempoOnda = 2,
        entradaOnda = 100
        }
        where i10 = inimigo10
              i11 = inimigo11
              i12 = inimigo12
