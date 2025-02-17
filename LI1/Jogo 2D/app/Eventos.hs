module Eventos where
import LI12425
import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers

import Data.Maybe (listToMaybe)

-- | Função que reage a um input do jogador
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (Char 'v') Down _ _) it | estadoMenu it == MenuCreditos = it { estadoMenu = MenuPrincipal }
                                               | estadoMenu it == MenuAjuda = it { estadoMenu = MenuPausa }
                                               | estadoMenu it == MenuSair = it { estadoMenu = MenuPrincipal }
                                               | estadoMenu it == MenuLoja = it { estadoMenu = Jogando }
                                               | otherwise = it

--Teclas para MenuPrincipal
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it | estadoMenu it == MenuInicial = it { estadoMenu = MenuPrincipal }                                           
                                                          | estadoMenu it == MenuPrincipal = it { estadoMenu = SelecionarNivel }
                                                          | estadoMenu it == MenuPausa = it { estadoMenu = Jogando }
                                                          | otherwise = it
reageEventos (EventKey (Char 'c') Down _ _) it | estadoMenu it == MenuPrincipal = it { estadoMenu = MenuCreditos }
                                               | otherwise = it
reageEventos (EventKey (Char 's') Down _ _) it | estadoMenu it == MenuPrincipal= it { estadoMenu = MenuSair }
                                               | otherwise = it

-- Teclas para menu Jogando
reageEventos (EventKey (Char 'h') Down _ _) it | estadoMenu it == MenuPausa = it { estadoMenu = MenuAjuda } --tirar quando o Menu Jogando estiver feito
                                               | otherwise = it
reageEventos (EventKey (Char 'p') Down _ _) it | estadoMenu it == Jogando = it { estadoMenu = MenuPausa }

reageEventos (EventKey (Char 'l') Down _ _) it | estadoMenu it == Jogando = it { estadoMenu = MenuLoja }
                                               |otherwise = it

                                                                  -- mouse MenuPrincipal
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it | estadoMenu it == MenuPrincipal, (-380) <= x, x <= 470, 170 <= y, y <= 380 = it { estadoMenu = SelecionarNivel }
                                                                  | estadoMenu it == MenuPrincipal, (-380) <= x, x <= 470, -130 <= y, y <= 100 = it { estadoMenu = MenuCreditos }
                                                                  | estadoMenu it == MenuPrincipal, (-380) <= x, x <= 470, -420 <= y, y <= -210 = it { estadoMenu = MenuSair }
                                                                  -- mouse SelecionarNivel
                                                                  | estadoMenu it == SelecionarNivel, (-450) <= x, x <= 450, 100 <= y, y <= 300 = it { estadoMenu = Jogando, jogo = jogoInicial }
                                                                  | estadoMenu it == SelecionarNivel, (-450) <= x, x <= 450, -300 <= y, y <= -100 = it { estadoMenu = Jogando2, jogo = jogo02}
                                                                  -- mouse MenuCreditos
                                                                  | estadoMenu it == MenuCreditos, (-150) <= x, x <= 200, -490 <= y, y <= -370 = it { estadoMenu = MenuPrincipal }
                                                                  -- mouse MenuSair
                                                                  | estadoMenu it == MenuSair, 100 <= x, x <= 650, -260 <= y, y <= -20 = it { estadoMenu = MenuPrincipal }
                                                                  -- mouse MenuInicial
                                                                  | estadoMenu it == MenuInicial = it { estadoMenu = MenuPrincipal }
                                                                  -- mouse Jogando
                                                                  | estadoMenu it == Jogando, (-1000) <= x, x <= (-810), 370 <= y, y <= 570 = it { estadoMenu = MenuPausa }
                                                                  | estadoMenu it == Jogando, 830 <= x, x <= 1000, 370 <= y, y <= 570 = it { estadoMenu = MenuLoja }
                                                                  | estadoMenu it == Jogando, 830 <= x, x <= 1000, -570 <= y,y <= -370 = it { estadoMenu = VenderTorre }
                                                                  | estadoMenu it == Jogando, velocidadeJogo it == 1,(-1000) <= x, x <= (-830), -570 <= y,y <= -370 = it { velocidadeJogo = 2 }
                                                                  | estadoMenu it == Jogando, velocidadeJogo it == 2,(-1000) <= x, x <= (-830), -570 <= y,y <= -370 = it { velocidadeJogo = 1 }
                                                                  -- mouse Jogando2
                                                                  | estadoMenu it == Jogando2, (-1000) <= x, x <= (-810), 370 <= y, y <= 570 = it { estadoMenu = MenuPausa }
                                                                  | estadoMenu it == Jogando2, 830 <= x, x <= 1000, 370 <= y, y <= 570 = it { estadoMenu = MenuLoja }
                                                                  | estadoMenu it == Jogando2, 830 <= x, x <= 1000, -570 <= y,y <= -370 = it { estadoMenu = VenderTorre }
                                                                  | estadoMenu it == Jogando2, velocidadeJogo it == 1,(-1000) <= x, x <= (-830), -570 <= y,y <= -370 = it { velocidadeJogo = 2 }
                                                                  | estadoMenu it == Jogando2, velocidadeJogo it == 2,(-1000) <= x, x <= (-830), -570 <= y,y <= -370 = it { velocidadeJogo = 1 }
                                                                  -- mouse MenuPausa
                                                                  | estadoMenu it == MenuPausa, (-450) <= x, x <= 490, 170 <= y, y <= 400 = it { estadoMenu = Jogando }
                                                                  | estadoMenu it == MenuPausa, (-450) <= x, x <= 490, -150 <= y, y <= 100 = it { estadoMenu = MenuPrincipal }
                                                                  | estadoMenu it == MenuPausa, (-950) <= x, x <= (-500), 300 <= y, y <= 500 = it { estadoMenu = MenuAjuda }
                                                                  -- mouse MenuAjuda
                                                                  | estadoMenu it == MenuAjuda, (-200) <= x, x <= 140, -490 <= y, y <= -390 = it { estadoMenu = MenuPausa }
                                                                  --mouse MenuLoja
                                                                  | estadoMenu it == MenuLoja, (-1000) <= x, x <= (-810), 370 <= y, y <= 570 = it { estadoMenu = MenuPausa } -- botao pausa
                                                                  | estadoMenu it == MenuLoja, 650 <= x, x <= 900, -490 <= y, y <= -400 = it { estadoMenu = Jogando } -- botao voltar
                                                                  | estadoMenu it == MenuLoja, x >= 630 && x <= 920 && y >= 100 && y <= 280, creditosBase (baseJogo (jogo it)) - 200 >= 0 = it { estadoMenu = ColocarTorre, novaTorre = Just torreFogo,jogo = (jogo it) { baseJogo = (baseJogo (jogo it)) { creditosBase = creditosBase (baseJogo (jogo it)) - 200 } } }
                                                                  | estadoMenu it == MenuLoja, x >= 630 && x <= 920 && y >= -100 && y <= 80, creditosBase (baseJogo (jogo it)) - 200 >= 0  = it { estadoMenu = ColocarTorre, novaTorre = Just torreGelo ,jogo = (jogo it) { baseJogo = (baseJogo (jogo it)) { creditosBase = creditosBase (baseJogo (jogo it)) - 200 } } }
                                                                  | estadoMenu it == MenuLoja, x >= 630 && x <= 920 && y >= -300 && y <= -120, creditosBase (baseJogo (jogo it)) - 150 >= 0  = it { estadoMenu = ColocarTorre,novaTorre = Just torreResina ,jogo = (jogo it) { baseJogo = (baseJogo (jogo it)) { creditosBase = creditosBase (baseJogo (jogo it)) - 150 } } }
                                                                  -- mouse ColocarTorre
                                                                  | estadoMenu it == ColocarTorre, torreColocavel (mapaJogo (jogo it)) (torresJogo (jogo it)) (coordenadasIsometricas (x, y)), Just torre <- novaTorre it = it { estadoMenu = Jogando, jogo = (jogo it){torresJogo = torre{posicaoTorre = (fromIntegral a, fromIntegral b)} : torresJogo (jogo it)} }
                                                                  -- mouse VenderTorre
                                                                  | estadoMenu it == VenderTorre, 830 <= x, x <= 1000, -570 <= y,y <= -370 = it { estadoMenu = Jogando }
                                                                  | estadoMenu it == VenderTorre, existeTorre (x, y) (torresJogo (jogo it)) = it {jogo = torreVendivel (x, y) (jogo it), estadoMenu = Jogando }
                                                                  | otherwise = it
                                                                  where (a, b) = coordenadasIsometricas (x, y)
                                                                  


reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it | estadoMenu it == MenuPausa = it { estadoMenu = MenuPrincipal }
                                                          | otherwise = it

reageEventos (EventKey (Char 'r') Down _ _) it | estadoMenu it == Perdeu = it { estadoMenu = MenuPrincipal }
                                               | estadoMenu it == Ganhou = it { estadoMenu = MenuPrincipal }
                                               | otherwise = it
reageEventos (EventKey (Char 'n') Down _ _) it 
                                            | estadoMenu it == Ganhou = it {jogo = jogo02 ,estadoMenu = Jogando2 }
                                            | otherwise = it
reageEventos _ it = it

-- | Converte coordenadas cartesianas para coordenadas do mapa (fix: corrigir valores)
coordenadasIsometricas :: (Float, Float) -> (Int, Int)
coordenadasIsometricas (x, y) = ((floor (8+(x / 128)-(y/72))), floor (8-(x / 128)-(y/72)))
-- >>> coordenadasIsometricas (390*2, 20*2)
-- (8,8)

torreColocavel :: Mapa -> [Torre] -> (Int, Int) -> Bool
torreColocavel mapa torres (a, b) = b >= 0 && b < length mapa && a >= 0 && a < length (head mapa) && case mapa !! b !! a of
                Relva -> not (any (\t -> posicaoTorre t == (fromIntegral a, fromIntegral b)) torres)
                Ponte1 -> not (any (\t -> posicaoTorre t == (fromIntegral a, fromIntegral b)) torres)
                Ponte2 -> not (any (\t -> posicaoTorre t == (fromIntegral a, fromIntegral b)) torres)
                _ -> False

existeTorre :: (Float, Float) -> [Torre] -> Bool
existeTorre (x, y) torres = any (\t -> (floor *** floor) (posicaoTorre t) == (coordenadasIsometricas (x, y))) torres
  where
    (***) f g (a, b) = (f a, g b)

torreVendivel :: (Float, Float) -> Jogo -> Jogo
torreVendivel (x, y) jogo = 
    let (a, b) = coordenadasIsometricas (x, y)
        torres = torresJogo jogo
        torreRemovida = listToMaybe [t | t <- torres, posicaoTorre t == (fromIntegral a, fromIntegral b)]
    in case torreRemovida of
        Just torre -> jogo {
            torresJogo = filter (\t -> posicaoTorre t /= (fromIntegral a, fromIntegral b)) torres,
            baseJogo = (baseJogo jogo) {creditosBase = creditosBase (baseJogo jogo) + valorTorre torre}
        }
        Nothing -> jogo
  where
    valorTorre :: Torre -> Int
    valorTorre torre | tipoProjetil (projetilTorre torre) == Fogo = 150
                     | tipoProjetil (projetilTorre torre) == Gelo = 150
                     | tipoProjetil (projetilTorre torre) == Resina = 100
                     | otherwise = 0