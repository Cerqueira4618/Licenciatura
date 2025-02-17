{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : João Pedro Cunha Cerqueira <a111753@alunos.uminho.pt>
              Guilherme António Fernandes Gomes <a110449@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2



-- | Função que atualiza o jogo após um determinado tempo, aplicando as funções da Tarefa3
atualizaJogo :: Tempo -> Jogo -> Jogo 
atualizaJogo n jogo = jogo 
    { inimigosJogo = colideBaseInimigo (baseJogo jogo) (atualizaInimigos n inimigosAtualizados)
    , baseJogo = atualizaCreditos inimigosAtualizados (colisaoBase (baseJogo jogo) inimigosAtualizados)
    , torresJogo = resetTempoTorre (atualizaTempoTorre n (torresJogo jogo))
    , portaisJogo = atualizaPortais n (portaisJogo jogo)
    , lojaJogo = lojaJogo jogo
    }
  where
    inimigosAtualizados = aplicaEfeitosProjeteis $ dispararTorres (torresJogo jogo) 
                          (atualizaPosicaoInimigos n 
                          (atribuiDirecaoInimigos 
                          (atualizaEfeitos 
                          (atDuracaoProjInimigos n  
                          (lancaInimigosPortais n (portaisJogo jogo) (inimigosJogo jogo)))) 
                          (mapaJogo jogo)))
                        
-- >>> atualizaJogo 0.2 (Jogo {mapaJogo = [[Terra,Terra],[Relva,Terra]], torresJogo = [Torre {posicaoTorre = (0.0,1.0), alcanceTorre = 5.0, rajadaTorre = 2, danoTorre = 1.0, cicloTorre = 1.5, tempoTorre = 0.8, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}}], inimigosJogo = [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}], baseJogo = Base {posicaoBase = (0.0,0.0), vidaBase = 10.0, creditosBase = 0}, portaisJogo = [Portal {posicaoPortal = (0.0,0.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}], cicloOnda = 1.0, tempoOnda = 0.0, entradaOnda = -1.0}]}], lojaJogo = [(1000, Torre {posicaoTorre = (0.0,1.0), alcanceTorre = 1.0, rajadaTorre = 2, danoTorre = 1.0, cicloTorre = 1.5, tempoTorre = 0.8, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}})]})
-- Jogo {baseJogo = Base {vidaBase = 10.0, posicaoBase = (0.0,0.0), creditosBase = 0}, portaisJogo = [Portal {posicaoPortal = (0.0,0.0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1.0, tempoOnda = 1.0, entradaOnda = -1.0}]}], torresJogo = [Torre {posicaoTorre = (0.0,1.0), danoTorre = 1.0, alcanceTorre = 5.0, rajadaTorre = 2, cicloTorre = 1.5, tempoTorre = 0.6, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}}], mapaJogo = [[Terra,Terra],[Relva,Terra]], inimigosJogo = [Inimigo {posicaoInimigo = (0.0,-0.2), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []},Inimigo {posicaoInimigo = (0.0,-0.2), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}], lojaJogo = [(1000,Torre {posicaoTorre = (0.0,1.0), danoTorre = 1.0, alcanceTorre = 1.0, rajadaTorre = 2, cicloTorre = 1.5, tempoTorre = 0.8, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}})]}

-- | Função que verifica se um inimigo está morto se sim remove-o da lista de inimigos
atualizaInimigos :: Tempo -> [Inimigo] -> [Inimigo]
atualizaInimigos _ [] = []
atualizaInimigos n (x:xs) 
    | vidaInimigo x <= 0 = atualizaInimigos n xs
    | otherwise = x: atualizaInimigos n xs

-- >>> atualizaInimigos 1 [Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=1.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}, Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=0.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}]
-- [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 1.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}]


-- | Função que adiciona o butim dos inimigos aos créditos da base do jogador
atualizaCreditos :: [Inimigo] -> Base -> Base
atualizaCreditos [] base = base
atualizaCreditos (x:xs) base = if length [x] > length (atualizaInimigos 0 [x]) -- Verifica se o inimigo está morto
                               then atualizaCreditos xs (base {creditosBase = creditosBase base + butimInimigo x})
                               else atualizaCreditos xs base

-- | Função que atualiza o tempo restante para a próxima rajada de tiros
atualizaTempoTorre :: Tempo -> [Torre] -> [Torre]
atualizaTempoTorre _ [] = []
atualizaTempoTorre t (x:xs) = x {tempoTorre = tempoTorre x - t} : atualizaTempoTorre t xs
-- >>> atualizaTempoTorre 0.2 [Torre {posicaoTorre = (0.0,0.0), alcanceTorre = 1.0, rajadaTorre = 2, danoTorre = 1.0, cicloTorre = 1.5, tempoTorre = 0.8, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}}]
-- [Torre {posicaoTorre = (0.0,0.0), danoTorre = 1.0, alcanceTorre = 1.0, rajadaTorre = 2, cicloTorre = 1.5, tempoTorre = 0.6, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}}]


-- | Função que atualiza o tempo restante após uma rajada da torre
resetTempoTorre :: [Torre] -> [Torre]
resetTempoTorre [] = []
resetTempoTorre (x:xs) = if tempoTorre x <= (-0.1) 
                         then x {tempoTorre = cicloTorre x} : resetTempoTorre xs
                         else x : resetTempoTorre xs
-- >>> resetTempoTorre [Torre {posicaoTorre = (0.0,0.0), alcanceTorre = 1.0, rajadaTorre = 2, danoTorre = 1.0, cicloTorre = 1.5, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}}]
-- [Torre {posicaoTorre = (0.0,0.0), danoTorre = 1.0, alcanceTorre = 1.0, rajadaTorre = 2, cicloTorre = 1.5, tempoTorre = 1.5, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}}]


-- | Função que atualiza um inimigo após uma rajada de tiros de torres
dispararTorre :: Torre -> [Inimigo] -> [Inimigo]
dispararTorre torre inimigos = if tempoTorre torre <= 0 then map (atingeInimigo torre) inimigosnoalcance else inimigos 
    where inimigosnoalcance = inimigosNoAlcance torre inimigos

-- >>> dispararTorre (Torre {posicaoTorre = (0.0,0.0), alcanceTorre = 1.0, danoTorre = 1.0, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1}}) [Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}, Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}]
-- [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 4.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}]},Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 4.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 1.0}]}]

-- | Função que atualiza todos os inimigos após uma rajada de tiros de torres
dispararTorres :: [Torre] -> [Inimigo] -> [Inimigo]
dispararTorres [] inimigos = inimigos
dispararTorres (torre:torres) inimigos = dispararTorres torres (map (aplicaDanoTorre torre) inimigos)

-- | Função que aplica o dano de uma torre a um inimigo
aplicaDanoTorre :: Torre -> Inimigo -> Inimigo
aplicaDanoTorre torre inimigo
    | distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre && tempoTorre torre <= 0 = inimigo 
        { vidaInimigo = vidaInimigo inimigo - danoTorre torre
        , projeteisInimigo = projetilTorre torre : projeteisInimigo inimigo
        }
    | otherwise = inimigo

-- | Função auxiliar que aplica o efeito do projétil de gelo a um inimigo
efeitoGelo :: Inimigo -> Inimigo
efeitoGelo inimigo = if existeGelo (projeteisInimigo inimigo)
                     then inimigo {velocidadeInimigo = 0}
                     else inimigo {velocidadeInimigo = velocidadeOriginal inimigo}
  where 
    existeGelo :: [Projetil] -> Bool -- Função que verifica se existe um projétil de gelo na lista de projéteis de um inimigo
    existeGelo [] = False
    existeGelo (x:xs) = (tipoProjetil x == Gelo) || existeGelo xs

    velocidadeOriginal :: Inimigo -> Float -- Função que retorna a velocidade original do inimigo
    velocidadeOriginal i = if any (\p -> tipoProjetil p == Gelo) (projeteisInimigo i)
                           then velocidadeInimigo i
                           else velocidadeInimigo i

-- | Função auxiliar que aplica o efeito do projétil de resina a um inimigo
efeitoResina :: Inimigo -> Inimigo
efeitoResina inimigo = if existeResina (projeteisInimigo inimigo)
                       then inimigo {velocidadeInimigo = 0.5}
                       else inimigo {velocidadeInimigo = velocidadeOriginal inimigo}
  where 
    existeResina :: [Projetil] -> Bool -- Função que verifica se existe um projétil de resina na lista de projéteis de um inimigo
    existeResina [] = False
    existeResina (x:xs) = (tipoProjetil x == Resina) || existeResina xs

    velocidadeOriginal :: Inimigo -> Float -- Função que retorna a velocidade original do inimigo
    velocidadeOriginal i = if any (\p -> tipoProjetil p == Resina) (projeteisInimigo i)
                           then velocidadeInimigo i / 0.6
                           else velocidadeInimigo i
{-
velocidadeOriginal :: Inimigo -> Float -- Função que retorna a velocidade original do inimigo
velocidadeOriginal i | butimInimigo i == 5 = 1.5
                     | butimInimigo i == 10 = 1
                     | butimInimigo i == 25 = 0.8
                     | otherwise = 1
-}
-- | Função auxiliar que aplica o efeito do projétil de fogo a um inimigo
efeitoFogo :: Inimigo -> Inimigo
efeitoFogo inimigo = if existeFogo (projeteisInimigo inimigo)
                       then inimigo {vidaInimigo = vidaInimigo inimigo * 0.995}
                       else inimigo
  where existeFogo :: [Projetil] -> Bool -- Função que verifica se existe um projétil de fogo na lista de projéteis de um inimigo
        existeFogo [] = False
        existeFogo (x:xs) = (tipoProjetil x == Fogo) || existeFogo xs

-- | Função auxiliar que atualiza a duração dos projéteis de um inimigo
atualizaDuracaoProjeteis :: Tempo -> [Projetil] -> [Projetil] 
atualizaDuracaoProjeteis _ [] = []
atualizaDuracaoProjeteis t (x:xs) = case duracaoProjetil x of
                                  Finita d | d > 0 -> x {duracaoProjetil = Finita (d - t)} : atualizaDuracaoProjeteis t xs
                                           | d <= 0 -> atualizaDuracaoProjeteis t xs
                                  _ -> atualizaDuracaoProjeteis t xs

-- | Função que atualiza a duração dos projéteis de todos os inimigos
atDuracaoProjInimigos :: Tempo -> [Inimigo] -> [Inimigo]
atDuracaoProjInimigos _ [] = []
atDuracaoProjInimigos t l = map (\x -> x {projeteisInimigo = atualizaDuracaoProjeteis t (projeteisInimigo x)}) l

-- | Função que aplica os efeitos dos projéteis aos inimigos
aplicaEfeitosProjeteis :: [Inimigo] -> [Inimigo]
aplicaEfeitosProjeteis = map (efeitoFogo . efeitoResina . efeitoGelo)


-- | Função que atualiza os efeitos dos projéteis dos inimigos
atualizaEfeitos :: [Inimigo] -> [Inimigo] 
atualizaEfeitos [] = []
atualizaEfeitos l = map (efeitoFogo . efeitoResina . efeitoGelo) l


-- | Função que verifica se algum inimigo ativo colidiu com a base do jogador, caso sim, a vida da base é reduzida pelo dano do inimigo
colisaoBase :: Base -> [Inimigo] -> Base
colisaoBase base [] = base
colisaoBase base (x:xs) = 
    if (floor (fst (posicaoBase base)) :: Int) == (floor (fst (posicaoInimigo x)) :: Int) && (floor (snd (posicaoBase base)) :: Int) == (floor (snd (posicaoInimigo x)) :: Int)
    then colisaoBase (base {vidaBase = vidaBase base - ataqueInimigo x}) xs
    else colisaoBase base xs

-- >>> colisaoBase (Base {posicaoBase = (0.0,0.0), vidaBase = 10.0, creditosBase = 0}) [Inimigo {posicaoInimigo = (0.9,0.6),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}]
-- Base {vidaBase = 8.0, posicaoBase = (0.0,0.0), creditosBase = 0}

-- | Função que remove os inimigos que colidiram com a base do jogador
colideBaseInimigo :: Base -> [Inimigo] -> [Inimigo]
colideBaseInimigo _ [] = []
colideBaseInimigo base (x:xs) = if (floor (fst (posicaoBase base)) :: Int) == (floor (fst (posicaoInimigo x)) :: Int) && (floor (snd (posicaoBase base)) :: Int) == (floor (snd (posicaoInimigo x)) :: Int)
                                then colideBaseInimigo base xs
                                else x : colideBaseInimigo base xs

-- | Função auxiliar que lança os inimigos de um portal, de acordo com as ondas
lancaInimigos :: Tempo -> Portal -> [Inimigo] -> [Inimigo]
lancaInimigos t portal inimigosAtivos 
    | null (ondasPortal portal) = inimigosAtivos
    | null (inimigosOnda (head (ondasPortal portal))) = lancaInimigos t (portal {ondasPortal = tail (ondasPortal portal)}) inimigosAtivos
    | tempoOnda (head (ondasPortal portal)) <= 0 && entradaOnda (head (ondasPortal portal)) <= 0 = lancaInimigos t (portal {ondasPortal = tail (ondasPortal portal)}) (inimigo1 : inimigosAtivos)
    | otherwise = lancaInimigos t (portal {ondasPortal = tail (ondasPortal portal)}) inimigosAtivos
    where inimigo1 = head (inimigosOnda (head (ondasPortal portal)))
-- >>> lancaInimigos 0.2 (Portal {posicaoPortal = (0.0,0.0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1.0, tempoOnda = 0.0, entradaOnda = -1.0}]}) []
-- []

-- | Função auxiliar que atualiza o tempo restante para a entrada de um novo inimigo de uma onda e remove o inimigo na cabeça da lista de inimigos da onda
atualizaTempoOnda :: Tempo -> Portal -> Portal
atualizaTempoOnda t portal = portal {ondasPortal = map atualizaTempoOndaAux (ondasPortal portal)}
    where atualizaTempoOndaAux :: Onda -> Onda
          atualizaTempoOndaAux onda
              | tempoOnda onda <= 0 && entradaOnda onda <= 0 && not (null (inimigosOnda onda)) = onda {tempoOnda = cicloOnda onda, inimigosOnda = tail (inimigosOnda onda)}
              | otherwise = onda {tempoOnda = tempoOnda onda - t}

-- | Função auxiliar que atualiza o tempo restante para a entrada de uma nova onda
atualizaEntradaOnda :: Tempo -> Portal -> Portal
atualizaEntradaOnda t portal = portal {ondasPortal = map atualizaEntradaOndaAux (ondasPortal portal)}
    where atualizaEntradaOndaAux :: Onda -> Onda
          atualizaEntradaOndaAux onda = if entradaOnda onda >= 0
                                        then onda {entradaOnda = entradaOnda onda - t}
                                        else onda

-- | Função que atualiza o tempo restante para a entrada de uma nova onda e o tempo restante para a entrada de um novo inimigo de uma onda da lista de portais
atualizaPortais :: Tempo -> [Portal] -> [Portal]
atualizaPortais _ [] = []
atualizaPortais t portais = map (atualizaTempoOnda t . atualizaEntradaOnda t) portais


-- | Função que lança os inimigos de todos os portais de acordo com as ondas
lancaInimigosPortais :: Tempo -> [Portal] -> [Inimigo] -> [Inimigo]
lancaInimigosPortais _ [] inimigosAtivos = inimigosAtivos
lancaInimigosPortais t (x:xs) inimigosAtivos = lancaInimigosPortais t xs (lancaInimigos t x inimigosAtivos)
-- >>> lancaInimigosPortais 0.2 [Portal {posicaoPortal = (0.0,0.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}], cicloOnda = 1.0, tempoOnda = 0.0, entradaOnda = -1.0}]}, Portal {posicaoPortal = (0.0,0.0), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}], cicloOnda = 1.0, tempoOnda = 0.0, entradaOnda = -1.0}]}] []
-- [Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []},Inimigo {posicaoInimigo = (0.0,0.0), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}]

-- | Função que atualiza a posição de todos os inimigos com o tempo, tendo em conta a direção e velocidade dos mesmos
atualizaPosicaoInimigos :: Tempo -> [Inimigo] -> [Inimigo]
atualizaPosicaoInimigos _ [] = []
atualizaPosicaoInimigos t l = map (atualizaPosicaoInimigo t) l
-- >>> atualizaPosicaoInimigos 0.2 [Inimigo {posicaoInimigo = (0.0,0.0),direcaoInimigo= Este, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}]
-- [Inimigo {posicaoInimigo = (0.2,0.0), direcaoInimigo = Este, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}]

-- | Funçao que atualiza a posição de um inimigo dado uma direção e o tempo passado, de acordo com a sua velocidade
atualizaPosicaoInimigo :: Tempo -> Inimigo ->Inimigo
atualizaPosicaoInimigo t i = case direcaoInimigo i of
                            Norte -> i {posicaoInimigo = (fst (posicaoInimigo i), snd (posicaoInimigo i) - velocidadeInimigo i * t)}
                            Sul -> i {posicaoInimigo = (fst (posicaoInimigo i), snd (posicaoInimigo i) + velocidadeInimigo i * t)}
                            Este -> i {posicaoInimigo = (fst (posicaoInimigo i) + velocidadeInimigo i * t, snd (posicaoInimigo i))}
                            Oeste -> i {posicaoInimigo = (fst (posicaoInimigo i) - velocidadeInimigo i * t, snd (posicaoInimigo i) )}

-- | Função auxiliar que atribui direção a um inimigo
atribuiDirecao :: Inimigo -> Mapa -> Inimigo
atribuiDirecao i mapa = i {direcaoInimigo = novaDirecao}
    where
        (x, y) = posicaoInimigo i
        novaDirecao
                | isTerra (floor x, floor y) = Norte
                | isTerra (floor (x + 1), floor (y + 1)) = Este
                | isTerra (floor x, floor (y + 1)) = Sul
                | isTerra (floor (x - 1), floor y) = Oeste
                | otherwise = direcaoInimigo i
        isTerra (a, b) = b >= 0 && b < length mapa && a >= 0 && a < length (head mapa) && case mapa !! b !! a of
                Terra -> True
                _ -> False
            
-- >>> atribuiDirecao (Inimigo {posicaoInimigo = (1.0,0.0),direcaoInimigo= Norte, vidaInimigo=5.0, velocidadeInimigo = 1.0, ataqueInimigo=2.0,butimInimigo=500,projeteisInimigo = []}) [[Terra,Terra],[Terra,Relva]]
-- Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Norte, vidaInimigo = 5.0, velocidadeInimigo = 1.0, ataqueInimigo = 2.0, butimInimigo = 500, projeteisInimigo = []}

-- | Função que atribui direção a todos os inimigos
atribuiDirecaoInimigos :: [Inimigo] -> Mapa -> [Inimigo]
atribuiDirecaoInimigos [] _ = []
atribuiDirecaoInimigos l mapa =  map (`atribuiDirecao` mapa) l

