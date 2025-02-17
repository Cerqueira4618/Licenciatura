module Desenhar where


import Graphics.Gloss
import ImmutableTowers
import LI12425
import Data.Maybe (fromMaybe)

-- | Função que desenha o jogo
draw :: [Picture] -> ImmutableTowers -> Picture
draw imagens@[relva,terra,agua,background, backgroundMenu,ponte1,ponte2,portal,base,pause,inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde,botaoLoja, torrefogo , torregelo,torreresina,botaoVender,botaoVelocidade] it 
  | estadoMenu it == MenuInicial = Pictures [background, molduraTexto, fundoTexto , pressEnter ] 
  | estadoMenu it == MenuPrincipal =  Pictures [ Scale 5 4.8 $ backgroundMenu,desenhaMenuPrincipal it]
  | estadoMenu it == SelecionarNivel = Pictures [Scale 5 4.8 $ backgroundMenu, Translate 0 200 $ color black $ rectangleSolid 910 210, Translate 0 200 $ color red $ rectangleSolid 900 200, Translate (-460) 350 $ Scale 1 1 $ color white $ Text "Escolha o nivel", Translate (-200) 150 $ Scale 1 1 $ color white $ Text "Nivel 1", Translate 0 (-200) $ color black $ rectangleSolid 910 210, Translate 0 (-200) $ color red $ rectangleSolid 900 200, Translate (-200) (-250) $ Scale 1 1 $ color white $ Text "Nivel 2"]
  | estadoMenu it == MenuCreditos = Pictures [Scale 5 4.8 $ backgroundMenu, desenhaMenuCreditos it]
  | estadoMenu it == MenuAjuda = Pictures [desenhaMapa it,desenhaMenuAjuda it]
  | estadoMenu it == MenuSair = Pictures [Scale 5 4.8 $ backgroundMenu,desenhaMenuSair it]
  | estadoMenu it == Jogando = Pictures [desenhaJogo imagens it, desenhaBotaoLoja botaoLoja, desenhaDinheiro it, desenhaBotaoVender botaoVender, desenhaBotaoVelocidade botaoVelocidade]
  | estadoMenu it == Jogando2 = Pictures [desenhaJogo imagens it, desenhaBotaoLoja botaoLoja, desenhaDinheiro it, desenhaBotaoVender botaoVender, desenhaBotaoVelocidade botaoVelocidade]   
  | estadoMenu it == MenuPausa = Pictures [desenhaMapa it, desenhaMenuPausa it]
  | estadoMenu it == MenuLoja = Pictures [desenhaJogo imagens it, desenhaMenuLoja [torrefogo,torregelo,torreresina] it, Translate 190 150 $ Scale 0.7 0.7 $ desenhaDinheiro it]
  | estadoMenu it == ColocarTorre = Pictures [color (makeColorI 87 185 255 255) $ Polygon [(-1000,1000), (-1000,-1000), (1000,-1000), (1000,1000)], desenhaMapaColocarTorre (mapaJogo (jogo it)) [relva,terra,agua,ponte1,ponte2], desenhaTorresColocarTorre (torresJogo (jogo it)) [torrefogo,torregelo,torreresina]]
  | estadoMenu it == VenderTorre = Pictures [desenhaVenderTorre [torrefogo,torregelo,torreresina] it, desenhaBotaoVender botaoVender]
  | estadoMenu it == Perdeu = Pictures [ Translate (-500) 0 $ Scale 2 2 $ Color red $ Text "Perdeu", Translate (-600) (-100) $ Scale 0.5 0.5 $ Color white $ Text "Pressione 'r' para voltar ao menu principal"]
  | estadoMenu it == Ganhou = Pictures [ Translate (-500) 0 $ Scale 2 2 $ Color green $ Text "Ganhou", Translate (-600) (-100) $ Scale 0.5 0.5 $ Color white $ Text "Pressione 'r' para voltar ao menu principal",Translate (-600) (-200) $ Scale 0.5 0.5 $ Color white $ Text "Para Jogar o Proximo Nivel Pressione 'n'",Translate (-600) (-350) $ Scale 0.4 0.4 $ Color white $ Text  "Caso esteja no nivel 2 e queira voltar a jogar clique 'n'"]
  | otherwise = background
  
draw [] _ = Blank
draw [_] _ = Blank
draw (_:_:_:_) _ = Blank
draw _ _ = Blank

-- | Função que desenha os elementos principais do jogo
desenhaJogo :: [Picture] -> ImmutableTowers -> Picture
desenhaJogo [relva,terra,agua,background, backgroundMenu,ponte1,ponte2,portal,base,pause,inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde,botaoLoja,torrefogo,torregelo,torreresina,botaoVender,botaoVelocidade] it =
                           Pictures [desenhaMapa it,
                          desenhaTorres [torrefogo,torregelo,torreresina] (torresJogo (jogo it)),
                          desenhaInimigos [inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde] (inimigosJogo (jogo it)), 
                          desenhaBase base (baseJogo (jogo it)), 
                          desenhaPortais portal (portaisJogo (jogo it)), 
                          desenhaBotaoPause pause,  
                          desenhaNumOndas (head (portaisJogo (jogo it)))]
desenhaJogo [] _ = Blank
desenhaJogo [_] _ = Blank
desenhaJogo (_:_:_:_) _ = Blank
desenhaJogo _ _ = Blank

-- | Função que desenha o mapa e o background do jogo
desenhaMapa :: ImmutableTowers -> Picture
desenhaMapa it = Pictures [color (makeColorI 87 185 255 255) $ Polygon [(-1000,1000), (-1000,-1000), (1000,-1000), (1000,1000)], desenhaMapaIsometrico (mapaJogo (jogo it)) (imagens it)]

-- | Texto que diz "PRESSIONE A TECLA ENTER"
pressEnter ::   Picture
pressEnter  = Translate (-500) (-400) $ Scale 0.5 0.5 $ Color yellow $ Text "PRESSIONE A TECLA ENTER"

-- | Fundo do texto
fundoTexto :: Picture
fundoTexto = Color red $ Polygon [(-530,-340), (-530,-410), (470,-410), (470,-340)]

-- | Moldura do texto
molduraTexto :: Picture
molduraTexto = Color (greyN 0.2) $ Polygon [(-540,-330), (-540,-420), (480,-420), (480,-330)]

-- | Função que desenha o Menu principal
desenhaMenuPrincipal :: ImmutableTowers -> Picture
desenhaMenuPrincipal it = if estadoMenu it == MenuPrincipal
                          then Pictures [
                            Color black $ Polygon [(-380, 380), (-380, 170), (470, 170), (470, 380)],
                            Color red $ Polygon [(-370, 370), (-370, 180), (460, 180), (460, 370)],
                            Color black $ Polygon [(-380, 100), (-380, -130), (470, -130), (470, 100)],
                            Color red $ Polygon [(-370, 90), (-370, -120), (460, -120), (460, 90)],
                            Color black $ Polygon [(-380, -210), (-380, -420), (470, -420), (470, -210)],
                            Color red $ Polygon [(-370, -220), (-370, -410), (460, -410), (460, -220)],
                            Translate (-360) 230 (Scale 1 1 $ Color white $ Text "Jogar (Enter)"),
                            Translate (-330) (-70) (Scale 1 1 $ Color white $ Text "Creditos (c)"),
                            Translate (-180) (-370) (Scale 1 1 $ Color white $ Text "Sair (s)")]
                          else Blank

-- | Função que desenha o Menu dos creditos
desenhaMenuCreditos :: ImmutableTowers -> Picture
desenhaMenuCreditos it = if estadoMenu it == MenuCreditos
                          then Pictures [Color (greyN 0.2) $ Polygon [(-1000,300), (-1000,-300), (1000,-300), (1000,300)],
                                         Translate (-700) 100 (Scale 1 1 $ Color white $ Text "Projeto realizado por:"), 
                                         Translate (-700) (-70) (Scale 0.8 0.8 $ Color white $ Text "Joao Pedro Cunha Cerqueira"), 
                                         Translate (-950) (-170) (Scale 0.8 0.8 $ Color white $ Text "Guilherme Antonio Fernandes Gomes"),
                                         Color (greyN 0.2) $ Polygon [(-150,-370), (-150,-490), (200,-490), (200,-370)],
                                         Translate (-140) (-460) (Scale 0.7 0.7 $ Color white $ Text "Voltar(v)")
                                          ]
                          else Blank

-- | Função que desenha o Menu de ajuda
desenhaMenuAjuda :: ImmutableTowers -> Picture
desenhaMenuAjuda it = if estadoMenu it == MenuAjuda
                          then Pictures [color (makeColorI 0 0 0 180) $ Polygon [(-1000,1000), (-1000,-1000), (1000,-1000), (1000,1000)],
                                         Translate (-150) 410 (Scale 0.8 0.8 $ Color white $ Text "Ajuda:"), 
                                         Translate (-930) 300 (Scale 0.45 0.6 $ Color white $ Text "O jogo consiste em construir torres para defender a sua base"), 
                                         Translate (-910) 200 (Scale 0.45 0.6 $ Color white $ Text "de inimigos que tentam destrui-la. Para construir torres"), 
                                         Translate (-910) 100 (Scale 0.45 0.6 $ Color white $ Text "basta clicar na torre desejada e clicar na posicao onde"), 
                                         Translate (-910) 0 (Scale 0.45 0.6 $ Color white $ Text "deseja coloca-la. Para vender uma torre basta clicar na"), 
                                         Translate (-910) (-100) (Scale 0.45 0.6 $ Color white $ Text "torre desejada e clicar no botao de vender."),
                                         Color black $ Polygon [(-200,-390), (-200,-490), (140,-490), (140,-390)],
                                         Color red $ Polygon [(-190,-400), (-190,-480), (130,-480), (130,-400)],
                                         Translate (-150) (-470) (Scale 0.7 0.6 $ Color white $ Text "Voltar")
                                          ]
                          else Blank

-- | Função que desenha o Menu de pausa
desenhaMenuPausa :: ImmutableTowers -> Picture
desenhaMenuPausa it = if estadoMenu it == MenuPausa
                          then Pictures [color (makeColorI 128 128 128 150) $ Polygon [(-1000,1000), (-1000,-1000), (1000,-1000), (1000,1000)],
                          Color black $ Polygon [(-450,400), (-450,170), (490,170), (490,400)],
                          Color red $ Polygon [(-430,380), (-430,190), (470,190), (470,380)],
                          Translate (-280) 230 (Scale 1 1 $ Color white $ Text "Continuar"), 
                          Color black $ Polygon [(-450,100), (-450,-150), (490,-150), (490,100)],
                          Color red $ Polygon [(-430,80), (-430,-130), (470,-130), (470,80)],
                          Translate (-430) (-70) (Scale 1 1 $ Color white $ Text "Menu Principal"),
                          Color black $ Polygon [(-450,-210), (-450,-450), (490,-450), (490,-210)],
                          Color red $ Polygon [(-430,-230), (-430,-430), (470,-430), (470,-230)],
                          Translate (-270) (-370) (Scale 1 1 $ Color white $ Text "Sair (esq)"),
                          Color black $ Polygon [(-950,500), (-950,300), (-500,300), (-500,500)],
                          Color red $ Polygon [(-930,480), (-930,320), (-520,320), (-520,480)],
                          Translate (-900) 360 (Scale 1 1 $ Color white $ Text "Ajuda")]
                          else Blank

-- | Função que desenha o Menu de sair do jogo
desenhaMenuSair :: ImmutableTowers -> Picture
desenhaMenuSair it = if estadoMenu it == MenuSair
              then Pictures [Color (greyN 0.2) $ Polygon [(-1000,200), (-1000,-300), (1000,-300), (1000,200)],
                     Translate (-910) 50 (Scale 1 1 $ Color white $ Text "Quer mesmo sair do jogo?"),
                     Color green $ Polygon [(-710,-20), (-710,-260), (-30,-260), (-30,-20)],
                     Color red $ Polygon [(100,-20), (100,-260), (650,-260), (650,-20)],
                     Translate (-700) (-190) (Scale 1 1 $ Color white $ Text "Sim (esc)  Nao (v)")]
              else Blank

-- | Função que desenha a loja do jogo
desenhaMenuLoja :: [Picture] -> ImmutableTowers -> Picture
desenhaMenuLoja (torrefogo: torregelo: torreresina:_) it =Pictures [color (makeColorI 0 0 0 180)  $ Polygon [(600,500), (600,-500), (950,-500), (950,500)],
                     Translate 710 440 (Scale 0.5 0.5 $ Color white $ Text "Loja"),
                     Color green $ Polygon [(650,-490), (650,-400), (900,-400), (900,-490)],
                     Color (makeColorI 0 0 0 100) $ Polygon [(630,100), (630,280), (920,280), (920,100)],
                     Translate 770 220 (Scale 1.2 1.2 $ torrefogo),
                     Translate 740 (-80) (Scale 0.2 0.2 $ Color white $ Text "200"),
                     Color (makeColorI 0 0 0 100) $ Polygon [(630,80), (630,-100), (920,-100), (920,80)],
                     Translate 770 20 (Scale 1.2 1.2 $ torregelo),
                     Translate 740 120 (Scale 0.2 0.2 $ Color white $ Text "200"),
                     Color (makeColorI 0 0 0 100) $ Polygon [(630,-120), (630,-300), (920,-300), (920,-120)],
                     Translate 770 (-180) (Scale 1.2 1.2 $ torreresina),
                     Translate 740 (-280) (Scale 0.2 0.2 $ Color white $ Text "150"), 
                     Translate 690 (-470) $ Scale 0.5 0.5 $ Color white $ Text "Voltar"]



-- | Função que desenha o Mapa do jogo
desenhaMapaIsometrico :: Mapa -> Imagens -> Picture
desenhaMapaIsometrico mapa imagens = Translate 0 446 $ Pictures (concatMap desenhaLinha (zip [0..] mapa))
  where
  desenhaLinha (y, linha) = map (desenhaTerrenoIsometrico y) (zip [0..] linha)
  desenhaTerrenoIsometrico y (x, terreno) = Scale 2 2 $ Translate (fromIntegral (x - y) * 32) (fromIntegral (x + y) * (-16)) (desenhaTerreno terreno imagens)

-- | Função auxiliar que desenha um terreno de acordo com o mapa 
desenhaTerreno :: Terreno -> Imagens -> Picture
desenhaTerreno terreno imagens = fromMaybe Blank (lookup (nomeTerreno terreno) imagens) 
-- a função fromMaybe devolve o segundo argumento se o primeiro for Nothing, caso contrário devolve o valor dentro do Just

-- | Função que relaciona um terreno a uma string com o nome desse terreno
nomeTerreno :: Terreno -> String
nomeTerreno Relva = "relva"
nomeTerreno Terra = "terra"
nomeTerreno Agua = "agua"
nomeTerreno Ponte1 = "ponte1"
nomeTerreno Ponte2 = "ponte2"

-- | Função que desenha a Base do jogo
desenhaBase :: Picture -> Base -> Picture
desenhaBase baseImagem base = Pictures [Scale 2 2 $ Translate x' y' baseImagem, desenhaVidaBase base]
  where
    (x, y) = (floor (fst (posicaoBase base)), floor (snd (posicaoBase base)))
    x' = fromIntegral (x * 32) - fromIntegral (y * 32) 
    y' = fromIntegral (y * (-16)) - fromIntegral (x * 16) + 286
    a = x' + 220
    b = y' + 280
    desenhaVidaBase :: Base -> Picture
    desenhaVidaBase base = Pictures [Translate a b $ Scale 1.3 1.3 $ Color black $ rectangleSolid (vidaBase base + 4) 24,Translate a b $ Scale 1.3 1.3 $ Color green $ rectangleSolid (vidaBase base) 20, Translate 415 415 $ Scale 0.15 0.15 $ Color white $ Text (show (vidaBase base))]

-- | Função auxiliar que desenha um portal
desenhaPortal :: Picture -> Portal -> Picture
desenhaPortal portalImagem p1 = Scale 2 2 $ Translate x' y' portalImagem 
  where
    (x, y) = (floor (fst (posicaoPortal p1)), floor (snd (posicaoPortal p1)))
    x' = fromIntegral (x * 32) - fromIntegral (y * 32) 
    y' = fromIntegral (y * (-16)) - fromIntegral (x * 16) + 270

-- | Função que desenha os portais do jogo
desenhaPortais :: Picture -> [Portal] -> Picture
desenhaPortais portalImagem ps = Pictures (map (desenhaPortal portalImagem) ps)

-- | Função auxiliar que desenha uma torre
desenhaTorre :: [Picture] -> Torre -> Picture
desenhaTorre (torreFogo:torreGelo:torreResina:_) t = Scale 2 2 $ Translate x' y' (case tipoProjetil (projetilTorre t) of
    Fogo -> torreFogo
    Gelo -> torreGelo
    Resina -> torreResina)
  where
    (x, y) = (floor (fst (posicaoTorre t)), floor (snd (posicaoTorre t)))
    x' = fromIntegral (x * 32) - fromIntegral (y * 32)
    y' = fromIntegral (y * (-16)) - fromIntegral (x * 16) + 270

-- | Função que desenha as torres do jogo
desenhaTorres :: [Picture] -> [Torre] -> Picture
desenhaTorres _ [] = Blank
desenhaTorres torreImagens ts = Pictures (map (desenhaTorre torreImagens) ts) 

-- | Função que desenha o botão de pause
desenhaBotaoPause :: Picture -> Picture
desenhaBotaoPause pauseImagem = Scale 2 2 $ Translate (-445) 220 pauseImagem

-- | Função que desenha o botão da loja
desenhaBotaoLoja :: Picture -> Picture
desenhaBotaoLoja botaoLoja = Scale 2 2 $ Translate 445 220 $ Color black $ botaoLoja

-- | Função que desenha os inimigos do jogo
desenhaInimigos :: [Picture] -> [Inimigo] -> Picture
desenhaInimigos _ [] = Blank
desenhaInimigos imagens is = Pictures (map (desenhaInimigo imagens) is)

-- | Função auxiliar que desenha um inimigo
desenhaInimigo :: [Picture] -> Inimigo -> Picture
desenhaInimigo imagens i = Scale 2 2 $ Translate x' y' (desenhaInimigoaux imagens i)
  where
    desenhaInimigoaux :: [Picture] -> Inimigo -> Picture
    desenhaInimigoaux (inimigoEAzul:inimigoNAzul:inimigoSAzul:inimigoOAzul:inimigoERoxo:inimigoNRoxo:inimigoSRoxo:inimigoORoxo:inimigoEVerde:inimigoNVerde:inimigoSVerde:inimigoOVerde:_) i
        | butimInimigo i == 5 = determinaDirecaoAzul inimigoEAzul inimigoNAzul inimigoSAzul inimigoOAzul i
        | butimInimigo i == 10 = determinaDirecaoRoxo inimigoERoxo inimigoNRoxo inimigoSRoxo inimigoORoxo i
        | otherwise = determinaDirecaoVerde inimigoEVerde inimigoNVerde inimigoSVerde inimigoOVerde i
    desenhaInimigoaux _ _ = Blank
    (x, y) = posicaoInimigo i
    x' =  (x * 32) - (y * 32)
    y' =  (y * (-16)) - (x * 16) + 250 


-- | Função auxiliar que desenha um inimigo azul na respetiva direção
determinaDirecaoAzul :: Picture -> Picture -> Picture -> Picture -> Inimigo -> Picture
determinaDirecaoAzul inimigoEAzul inimigoNAzul inimigoSAzul inimigoOAzul i 
                   | direcaoInimigo i == Este  =  inimigoEAzul 
                   | direcaoInimigo i == Norte =  inimigoNAzul 
                   | direcaoInimigo i == Sul =  inimigoSAzul 
                   | otherwise = inimigoOAzul 

-- | Função auxiliar que desenha um inimigo roxo na respetiva direção
determinaDirecaoRoxo :: Picture -> Picture -> Picture -> Picture -> Inimigo -> Picture
determinaDirecaoRoxo inimigoERoxo inimigoNRoxo inimigoSRoxo inimigoORoxo i 
           | direcaoInimigo i == Este  =  inimigoERoxo 
           | direcaoInimigo i == Norte =  inimigoNRoxo 
           | direcaoInimigo i == Sul =  inimigoSRoxo 
           | otherwise =  inimigoORoxo 

-- | Função auxiliar que desenha um inimigo verde na respetiva direção
determinaDirecaoVerde :: Picture -> Picture -> Picture -> Picture -> Inimigo -> Picture
determinaDirecaoVerde inimigoEVerde inimigoNVerde inimigoSVerde inimigoOVerde i 
           | direcaoInimigo i == Este  =  inimigoEVerde
           | direcaoInimigo i == Norte =  inimigoNVerde 
           | direcaoInimigo i == Sul =  inimigoSVerde 
           | otherwise =  inimigoOVerde 


-- | Função auxilair que conta quantas ondas de um portal têm o parâmetro entradaOnda menor ou igual a 0.
contaOndas :: Portal -> Int
contaOndas portal = length $ filter (\onda -> entradaOnda onda <= 0) (ondasPortal portal)

-- | Função que desenha o número de ondas de um portal, de acordo com a função contaOndas
desenhaNumOndas :: Portal -> Picture  
desenhaNumOndas portal = Translate (-950) 330 (Scale 0.4 0.4 (Color white (Text ("Onda: " ++ show (contaOndas portal)))))

-- | Função que desenha o dinheiro do jogo
desenhaDinheiro :: ImmutableTowers -> Picture
desenhaDinheiro it = Pictures [Translate 700 320 $ Scale 0.5 0.5 $ Color white $ Text "Dinheiro:",
                               Translate 700 260 $ Scale 0.5 0.5 $ Color white $ Text (show (creditosBase (baseJogo (jogo it))))]

-- | Função que desenha o mapa do jogo ao colocar uma torre, sendo a relva, a ponte1 e a ponte2 os únicos terrenos onde se pode colocar torres, por isso são desenhados 10 pixeis acima do resto do terreno
desenhaMapaColocarTorre :: Mapa -> [Picture] -> Picture
desenhaMapaColocarTorre mapa imagens = Translate 0 446 $ Pictures (concatMap desenhaLinha (zip [0..] mapa))
  where
  imagensAssociadas = zip ["relva", "terra", "agua", "ponte1", "ponte2"] imagens
  desenhaLinha (y, linha) = map (desenhaTerrenoIsometrico y) (zip [0..] linha)
  desenhaTerrenoIsometrico y (x, terreno) = Scale 2 2 $ Translate (fromIntegral (x - y) * 32) (fromIntegral (x + y) * (-16) + offsetY terreno) (desenhaTerreno terreno imagensAssociadas)
  offsetY Relva = 10
  offsetY Ponte1 = 10
  offsetY Ponte2 = 10
  offsetY _ = 0

-- | Função que desenha o botão de vender
desenhaBotaoVender :: Picture -> Picture 
desenhaBotaoVender imagem = Translate 890 (-440) $ Scale 2 2 $ imagem

-- | Função que desenha o mapa e as torres do jogo ao vender uma torre
desenhaVenderTorre :: [Picture] -> ImmutableTowers -> Picture
desenhaVenderTorre imagens it = Pictures [desenhaMapa it,
                                  desenhaTorres imagens (torresJogo (jogo it))]

-- | Função que desenha as torres ao colocar uma torre
desenhaTorresColocarTorre :: [Torre] -> [Picture] -> Picture
desenhaTorresColocarTorre torres imagensTorre = Pictures (map (Translate 0 20 . desenhaTorre imagensTorre) torres)

-- | Função que desenha o botão de velocidade, que acelera ou abranda o jogo
desenhaBotaoVelocidade :: Picture -> Picture
desenhaBotaoVelocidade imagem = Translate (-890) (-440) $ Scale 2 2 $ imagem