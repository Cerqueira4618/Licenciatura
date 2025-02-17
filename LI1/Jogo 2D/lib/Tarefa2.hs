{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : João Pedro Cunha Cerqueira <a111753@alunos.uminho.pt>
              Guilherme António Fernandes Gomes <a110449@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425


-- | Função que calcula a distância entre duas posições
distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


-- | A função vai filtrar os inimigos que estão no raio de alcance da torre
-- * Os inimigos que estiverem fora do alcanse da torre são colocados fora da lista de inimigos
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre)


-- | Função que faz com que a torre ataque o inimigo, atualizando a vida do inimigo e a lista de projéteis
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo = 
  inimigo 
    { -- Atualiza a vida do inimigo subtraindo o dano da torre
      vidaInimigo = vidaInimigo inimigo - danoTorre torre,
      
      -- Atualiza a lista de projéteis considerando sinergias
      projeteisInimigo = atualizaProjeteis (projetilTorre torre) (projeteisInimigo inimigo)
    }


-- | Função que verifica a existência de sinergias 
atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis novoProjetil [] = [novoProjetil]
atualizaProjeteis novoProjetil projeteis
--caso se existir projeteis de Gelo e Fogo
    | tipoProjetil novoProjetil == Gelo && or (existe Fogo projeteis) = projeteis 
    | tipoProjetil novoProjetil == Fogo && or (existe Gelo projeteis) = projeteis
-- caso Fogo e Resina
    | tipoProjetil novoProjetil == Fogo && or (existe Resina projeteis) = 
        dobrarDuracao (tipoProjetil novoProjetil) [novoProjetil] ++ projeteis
    | tipoProjetil novoProjetil == Resina && or (existe Fogo projeteis) = dobrarDuracao Fogo projeteis
--caso projeteis repetidos
    | or (existe (tipoProjetil novoProjetil) projeteis) = dobrarDuracao (tipoProjetil novoProjetil) projeteis
    | otherwise = novoProjetil : projeteis  -- Se nenhuma das condições anteriores for satisfeita

-- >>> atualizaProjeteis (Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}) [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}]
-- [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}]


-- | Função que dobra a duração de projéteis de um tipo específico
dobrarDuracao :: TipoProjetil -> [Projetil] -> [Projetil]
dobrarDuracao _ [] = []  -- Caso base: lista vazia, retorna lista vazia
dobrarDuracao tipo (p:ps)
  | tipoProjetil p == tipo = Projetil 
                             { tipoProjetil = tipoProjetil p
                             , duracaoProjetil = dobrarDuracaoDuracao (duracaoProjetil p)
                             } : dobrarDuracao tipo ps
  | otherwise = p : dobrarDuracao tipo ps

-- | Função auxiliar para dobrar a duração de um projétil
dobrarDuracaoDuracao :: Duracao -> Duracao
dobrarDuracaoDuracao (Finita t) = Finita (2 * t)  -- Se for uma duração finita, dobra o tempo
dobrarDuracaoDuracao Infinita = Infinita  -- Se a duração for infinita, mantém infinita



-- | Função para verificar a existência de um dado tipo de Projetil
existe :: TipoProjetil-> [Projetil]->[Bool]
existe _ [] = [False]
existe  tipo (x:xs) 
    |tipoProjetil x == tipo = [True]
    |otherwise = existe tipo xs


-- | Função que dado um portal e uma lista de inimigos vai colocar os inimigos na lista de inimigos ativos do portal
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigos = 
  (portal {ondasPortal = ativaOndas (ondasPortal portal)}, inimigos ++ criaInimigos (ondasPortal portal))
  where
    ativaOndas :: [Onda] -> [Onda] -- função que vai ativar ondas
    ativaOndas [] = []
    ativaOndas (onda:ondas) = 
      if entradaOnda onda <= 0
        then onda {entradaOnda = entradaOnda onda - 1} : ativaOndas ondas
        else onda : ativaOndas ondas

    criaInimigos :: [Onda] -> [Inimigo] -- função que vai criar a lista de inimigos ativos
    criaInimigos [] = []
    criaInimigos (onda:ondas) = 
      if entradaOnda onda == 0
        then inimigosOnda onda ++ criaInimigos ondas
        else criaInimigos ondas


-- >>> ativaInimigo (Portal {posicaoPortal = (1.0,1.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {vidaInimigo = 50,velocidadeInimigo = 10,ataqueInimigo = 20 ,butimInimigo = 50,projeteisInimigo =[],posicaoInimigo = (1.5,1.5),direcaoInimigo = Sul}], cicloOnda = 1.0, tempoOnda = 1.0, entradaOnda = 0.0}]}) [] 
-- (Portal {posicaoPortal = (1.0,1.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.5,1.5), direcaoInimigo = Sul, vidaInimigo = 50.0, velocidadeInimigo = 10.0, ataqueInimigo = 20.0, butimInimigo = 50, projeteisInimigo = []}], cicloOnda = 1.0, tempoOnda = 1.0, entradaOnda = -1.0}]},[Inimigo {posicaoInimigo = (1.5,1.5), direcaoInimigo = Sul, vidaInimigo = 50.0, velocidadeInimigo = 10.0, ataqueInimigo = 20.0, butimInimigo = 50, projeteisInimigo = []}])



-- | Função que verifica se o jogo terminou
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

-- >>> terminouJogo (Jogo {baseJogo = Base {posicaoBase = (1.0,1.0), vidaBase = 10.0}, portaisJogo = [Portal {posicaoPortal = (1.0,1.0), ondasPortal = []}]})
-- True

-- | Função que verifica se a lista de ondas de todos os portais está vazia
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) 

-- >>> ganhouJogo (Jogo {baseJogo = Base {posicaoBase = (1.0,1.0), vidaBase = 10.0}, portaisJogo = [Portal {posicaoPortal = (1.0,1.0), ondasPortal = []}],inimigosJogo=[]})
-- True

-- | Função que verifica se a vida da base é 0
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = case vidaBase (baseJogo jogo) of
  0 -> True
  _ -> False

-- >>> perdeuJogo (Jogo {baseJogo = Base {posicaoBase = (1.0,1.0), vidaBase = 0.0}, portaisJogo = []})
-- True
