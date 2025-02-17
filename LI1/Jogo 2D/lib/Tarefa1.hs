{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : João Pedro Cunha Cerqueira <a111753@alunos.uminho.pt>
              Guilherme António Fernandes Gomes <a110449@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425





-- | Verifica se um estado de jogo é válido
validaJogo :: Jogo -> Bool
validaJogo (Jogo base portais torres mapa inimigos _) = existePortal portais 
                                                         && sobreTerra portais mapa 
                                                         && verificaCaminho mapa portais base 
                                                         && naoSobrepostosPortais portais torres base
                                                         && ondaAtiva portais
                                                         && verificaInimigos portais
                                                         && inimigosSobreTerra inimigos mapa 
                                                         && inimigosNaoSobreTorre inimigos torres 
                                                         && veloPosi inimigos
                                                         && projeteisNormalizados inimigos
                                                         && torresSobreRelva torres mapa
                                                         && alcancePositivo torres
                                                         && rajadaPositiva torres
                                                         && cicloNaoNegativo torres
                                                         && torresNaoSobrepostas torres
                                                         && baseSobreTerra base mapa 
                                                         && creditosPositivos base 
                                                         && baseNaoSobreposta base portais torres

-- >>> validaJogo jogo01
-- True




-- | Verifica se existe um portal
existePortal :: [Portal] -> Bool 
existePortal l | null l = False 
               | otherwise = True

-- >>> existePortal portaisExemplo
-- True



-- | Verifica se os portais estão posicionados sobre a terra
sobreTerra :: [Portal] -> Mapa -> Bool
sobreTerra [] _ = True
sobreTerra (Portal (x, y) _:ps) mapa = (tipoMapa (x, y) mapa == Terra) && sobreTerra ps mapa


-- | Função auxiliar para que verifica o tipo de terreno do mapa numa determinada posição
tipoMapa :: Posicao -> Mapa -> Terreno
tipoMapa (x, y) ((h:t):l) | floor x == 0 && floor y == 0 = h
                          | floor x == 0 = tipoMapa (0, y - 1) l
tipoMapa (x, y) ((_:t):l) = tipoMapa (x - 1, y) (t : map tail l)
tipoMapa _ _ = error "Posição fora do mapa"

-- >>> sobreTerra [(Portal (2.5,1.1) [(Onda [(Inimigo (2,1) Este 120 2 10 15 [])] 2 2 15)])] [[Relva,Relva,Relva],[Agua,Agua,Terra]]
-- True




-- | Verifica se existe pelo menos um caminho (de terra) ligando um portal à base.
verificaCaminho :: Mapa -> [Portal] -> Base -> Bool
verificaCaminho mapa portais (Base _ posBase _) =
    any (\portal -> existeCaminho mapa (convertePosicao (posicaoPortal portal)) (convertePosicao posBase)) portais
  where
    -- Verifica se há um caminho de Terra entre duas posições
    existeCaminho :: Mapa -> (Int, Int) -> (Int, Int) -> Bool
    existeCaminho mapa inicio destino =
        dfs mapa inicio destino []
    -- Busca em Profundidade (DFS)
    dfs :: Mapa -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Bool
    dfs mapa (x, y) destino visitados
        | not (dentroDoMapa mapa (x, y)) = False                      -- Fora do mapa
        | (x, y) `elem` visitados = False                            -- Já visitado
        | mapa !! x !! y /= Terra = False                            -- Não é Terra
        | (x, y) == destino = True                                   -- Destino alcançado
        | otherwise =
            or [ dfs mapa (nx, ny) destino ((x, y) : visitados)       -- Recursão
               | (nx, ny) <- vizinhos (x, y) ]
    -- Verifica se uma posição está dentro dos limites do mapa
    dentroDoMapa :: Mapa -> (Int, Int) -> Bool
    dentroDoMapa mapa (x, y) = x >= 0 && y >= 0 && x < length mapa && y < length (head mapa)
    -- Calcula os vizinhos (acima, abaixo, esquerda, direita)
    vizinhos :: (Int, Int) -> [(Int, Int)]
    vizinhos (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    -- Converte uma posição (Float, Float) para o índice correspondente no mapa
    convertePosicao :: Posicao -> (Int, Int)
    convertePosicao (x, y) = (floor y, floor x) -- Inverte porque listas são acessadas por linha (y) e coluna (x)

-- >>> verificaCaminho mapa01 portaisExemplo (Base 120 (5.5,2.5) 10)
-- True



-- | Verifica se os portais estão sobrepostos a torres ou à base, caso estejam retorna False
naoSobrepostosPortais :: [Portal] -> [Torre] -> Base -> Bool
naoSobrepostosPortais portais torres base =
    not (any (`comparaPosTorre` torres) portais) && not (any (`comparaPosBase` base) portais)

-- | Compara a posição de um portal com a posição de todas as torres
comparaPosTorre :: Portal -> [Torre] -> Bool
comparaPosTorre (Portal (x, y) _) [] = False
comparaPosTorre (Portal (x, y) ond) (Torre (x1, y1) _ _ _ _ _ _ : ts)
        | floor x == floor x1 && floor y == floor y1 = True
        | otherwise = comparaPosTorre (Portal (x, y) ond) ts
comparaPosBase :: Portal -> Base -> Bool
comparaPosBase (Portal (x, y) _) (Base _ (x1, y1) _)
        | floor x == floor x1 && floor y == floor y1 = True
        | otherwise = False

-- >>> naoSobrepostosPortais [(Portal (1,1) [(Onda [(Inimigo (2,1) Este 120 2 10 15 [])] 2 2 15)])] [(Torre (2,1) 1 1 1 1 1 (Projetil Fogo Infinita)),(Torre (0,0) 2 2 2 2 2 (Projetil Gelo Infinita))] (Base 120 (1,2) 10)
-- True



-- | Verifica se há, no máximo, uma onda ativa por portal.
ondaAtiva :: [Portal] -> Bool
ondaAtiva [] = True  -- Se não há portais, a condição é satisfeita
ondaAtiva ((Portal _ ondas):portais) =
    (quantidadeOndasAtivas ondas <= 1) && ondaAtiva portais
  where
    -- Conta quantas ondas estão ativas (ou seja, entradaOnda <= 0)
    quantidadeOndasAtivas :: [Onda] -> Int
    quantidadeOndasAtivas [] = 0
    quantidadeOndasAtivas (Onda _ _ _ entradaOnda:resto)
        | entradaOnda <= 0 = 1 + quantidadeOndasAtivas resto  -- Contabiliza onda ativa
        | otherwise = quantidadeOndasAtivas resto            -- Ignora ondas inativas

-- >>> ondaAtiva [(Portal (1,1) [(Onda [(Inimigo (2,1) Este 120 2 10 15 [])] 2 2 15)])]
-- True



-- | Verifica se todos os inimigos por lançar têm a posição do respetivo portal, nı́vel de vida positivo, e lista de projéteis ativos vazia.
verificaInimigos :: [Portal] -> Bool
verificaInimigos [] = True
verificaInimigos (Portal posPortal ondas:portais) =
    all (verificaOnda posPortal) ondas && verificaInimigos portais
  where
    -- Verifica todos os inimigos de uma onda
    verificaOnda :: Posicao -> Onda -> Bool
    verificaOnda posPortal (Onda inimigos _ _ _) = all (verificaInimigo posPortal) inimigos
    -- Verifica as condições de um único inimigo
    verificaInimigo :: Posicao -> Inimigo -> Bool
    verificaInimigo posPortal inimigo =
        posicaoInimigo inimigo == posPortal &&  -- Posição do inimigo corresponde ao portal
        vidaInimigo inimigo > 0 &&             -- Vida do inimigo é positiva
        null (projeteisInimigo inimigo)        -- Lista de projéteis está vazia



-- | Verifica se todos os inimigos em jogo encontram-se sobre terra.
inimigosSobreTerra :: [Inimigo] -> Mapa -> Bool 
inimigosSobreTerra [] _ = True
inimigosSobreTerra (Inimigo (x,y) _ _ _ _ _ _:t) mapa = (tipoMapa (x,y) mapa == Terra) && inimigosSobreTerra t mapa

-- >>> inimigosSobreTerra [Inimigo {posicaoInimigo = (1.5,1.5),direcaoInimigo = Sul},Inimigo {posicaoInimigo = (2.5,4.5), direcaoInimigo = Este}] mapa01
-- True


-- | Verifica se os inimigos não estão sobrepostos a torres
inimigosNaoSobreTorre :: [Inimigo] -> [Torre] -> Bool 
inimigosNaoSobreTorre inimigos torres = not (any (`comparaInimigoTorre` torres) inimigos) 
    where
      comparaInimigoTorre :: Inimigo -> [Torre] -> Bool
      comparaInimigoTorre (Inimigo (x,y) _ _ _ _ _ _) [] = False
      comparaInimigoTorre (Inimigo (x,y) dir vida vel ataq butim proj) (Torre (x1, y1) _ _ _ _ _ _ : ts)
          | floor x == floor x1 && floor y == floor y1 = True
          | otherwise = comparaInimigoTorre (Inimigo (x, y) dir vida vel ataq butim proj) ts

-- >>> inimigosNaoSobreTorre [Inimigo {posicaoInimigo = (1.5,1.5),direcaoInimigo = Sul},Inimigo {posicaoInimigo = (2.5,4.5), direcaoInimigo = Este}] [(Torre (2,1) 1 1 1 1 1 (Projetil Fogo Infinita)),(Torre (0,0) 2 2 2 2 2 (Projetil Gelo Infinita))] 
-- True



-- | Verifica se a velocidade de todos os inimigos é positiva ou 0 (não pode ser negativa, caso seja devolve False)
veloPosi :: [Inimigo] -> Bool 
veloPosi [] = True
veloPosi ((Inimigo _ _ _ velocidade _ _ _):t) = velocidade >= 0 && veloPosi t



-- | Verifica se a lista de projéteis de todos os inimigos está normalizada, isto é um inimigo apenas contém um projétil de cada tipo e não contém simultaneamente, projéteis do tipo Fogo e Resina, nem Fogo e Gelo.
projeteisNormalizados :: [Inimigo] -> Bool
projeteisNormalizados = all projeteisDoInimigoNormalizados
  where
    -- Verifica se os projéteis de um único inimigo estão normalizados
    projeteisDoInimigoNormalizados :: Inimigo -> Bool
    projeteisDoInimigoNormalizados inimigo =
        let tipos = map tipoProjetil (projeteisInimigo inimigo)
        in semDuplicados tipos && semConflitos tipos
    -- Verifica se não há duplicados do mesmo tipo
    semDuplicados :: [TipoProjetil] -> Bool
    semDuplicados tipos = length tipos == length (removeDuplicados tipos)
    -- Remove duplicados de uma lista
    removeDuplicados :: Eq a => [a] -> [a]
    removeDuplicados = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
    -- Verifica se não há projéteis conflitantes
    semConflitos :: [TipoProjetil] -> Bool
    semConflitos tipos =
        not (Fogo `elem` tipos && Gelo `elem` tipos) &&
        not (Fogo `elem` tipos && Resina `elem` tipos)



-- | Verifica se todas as torres estão posicionadas sobre a relva
torresSobreRelva :: [Torre] -> Mapa -> Bool
torresSobreRelva [] _ = True
torresSobreRelva ((Torre (x,y) _ _ _ _ _ _):t) mapa = ((tipoMapa (x,y) mapa) == Relva) &&  torresSobreRelva t mapa

-- >>> torresSobreRelva [Torre {posicaoTorre = (4.5,3.5),projetilTorre = Projetil{tipoProjetil = Resina}},Torre {posicaoTorre = (0.5,2.5), projetilTorre = Projetil{tipoProjetil = Gelo}}] mapa01
-- True

 
-- | Verifica se o alcance de todas as torres da lista é positvo
alcancePositivo :: [Torre] -> Bool 
alcancePositivo [] = True 
alcancePositivo ((Torre _ _ alcance _ _ _ _):t) = alcance > 0 && alcancePositivo t

-- | Verifica se a rajada de todas as torres da lista é um valor positivo.
rajadaPositiva :: [Torre] -> Bool 
rajadaPositiva [] = True 
rajadaPositiva ((Torre _ _ _ raj _ _ _):t) = raj > 0 && rajadaPositiva t

-- | Verifica se o ciclo é um valor não negativo, para todas as torres da lista
cicloNaoNegativo :: [Torre] -> Bool
cicloNaoNegativo [] = True
cicloNaoNegativo ((Torre _ _ _ _ ciclo _ _):t) = ciclo >= 0 && cicloNaoNegativo t

-- | Verifica se nenhuma torre da lista está sobreposta
torresNaoSobrepostas :: [Torre] -> Bool
torresNaoSobrepostas [] = True
torresNaoSobrepostas [t1] = True
torresNaoSobrepostas (t1:t) = torreNaoSobreposta t1 t && torresNaoSobrepostas t
  where
    -- Verifica se uma torre não está sobreposta a nenhuma outra na lista
    torreNaoSobreposta :: Torre -> [Torre] -> Bool
    torreNaoSobreposta torre [] = True
    torreNaoSobreposta (Torre (x, y) _ _ _ _ _ _) ((Torre (a, b) _ _ _ _ _ _):ts)
        | floor x == floor a && floor y == floor b = False -- Sobreposição detectada
        | otherwise = torreNaoSobreposta (Torre (x, y) undefined undefined undefined undefined undefined undefined) ts

-- >>> torresNaoSobrepostas [Torre {posicaoTorre = (4.5,3.5),projetilTorre = Projetil{tipoProjetil = Resina}},Torre {posicaoTorre = (0.5,2.5), projetilTorre = Projetil{tipoProjetil = Gelo}}]
-- True



-- | Verifica se a base está posicionada sobre terra
baseSobreTerra :: Base -> Mapa -> Bool 
baseSobreTerra (Base _ (x,y) _) mapa = tipoMapa (x, y) mapa == Terra
-- >>> baseSobreTerra (Base undefined (5.5,2.5) undefined) mapa01
-- True

-- | Verifica se a base não tem créditos negativos
creditosPositivos :: Base -> Bool 
creditosPositivos (Base _ _ cred) = cred >= 0

-- | Verifica se a base não está sobreposta a um portal ou a uma torre
baseNaoSobreposta :: Base -> [Portal] -> [Torre] -> Bool 
baseNaoSobreposta base portais torres = not (any (`comparaPosBase` base) portais) && not (comparaBaseTorre base torres)
  where 
    comparaBaseTorre :: Base -> [Torre] -> Bool
    comparaBaseTorre (Base _ (x, y) _) [] = False
    comparaBaseTorre (Base vida (x, y) cred) (Torre (x1, y1) _ _ _ _ _ _ : ts)
          | floor x == floor x1 && floor y == floor y1 = True
          | otherwise = comparaBaseTorre (Base vida (x, y) cred) ts


-- | Exemplo de um jogo válido, para testar a função 'validaJogo'
jogo01 :: Jogo
jogo01 = Jogo {
     baseJogo
          = Base {posicaoBase = (5.5,2.5), creditosBase = 200},
     portaisJogo = [Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Este, vidaInimigo = 120, velocidadeInimigo = 2, ataqueInimigo = 10, butimInimigo = 15, projeteisInimigo = []}], cicloOnda = 2, tempoOnda = 2, entradaOnda = 15}]}],
     torresJogo = [
                   Torre {
                          posicaoTorre = (4.5,3.5),
                          projetilTorre = Projetil{tipoProjetil = Resina},
                          alcanceTorre = 2.5,
                          rajadaTorre = 3,
                          cicloTorre = 0.5},
                                          Torre {
                                                 posicaoTorre = (0.5,2.5),
                                                 projetilTorre = Projetil{tipoProjetil = Gelo},
                                                 alcanceTorre = 3.5,
                                                 rajadaTorre = 2,
                                                 cicloTorre = 0.3}
                  ],
     mapaJogo = mapa01,
     inimigosJogo = [
                     Inimigo {
                              posicaoInimigo = (1.5,1.5),
                              direcaoInimigo = Sul,
                              velocidadeInimigo = 1.2,
                              projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}]},
                     Inimigo {
                              posicaoInimigo = (2.5,4.5),
                              direcaoInimigo = Este,
                              velocidadeInimigo = 1.8,
                              projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Infinita}, Projetil {tipoProjetil = Resina, duracaoProjetil = Finita 4}]}
                    ]
                }

-- | Exemplo de um mapa válido, para testar as funções
mapa01 :: Mapa
mapa01 = [ [t, t, r, a, a, a],
           [r, t, r, a, r, r],
           [r, t, r, a, r, t],
           [r, t, r, a, r, t],
           [r, t, t, t, t, t],
           [a, a, a, a, r, r]
         ]
  where
   t = Terra
   r = Relva
   a = Agua

-- | Exemplo de portais válidos, para testar as funções
portaisExemplo :: [Portal]
portaisExemplo =
    [ Portal { posicaoPortal = (0.5,0.5), ondasPortal = [] },
      Portal { posicaoPortal = (1.5,1.5), ondasPortal = [] }]
