{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425
import Graphics.Gloss.Interface.Pure.Game

-- | Janela do jogo
janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

-- | Cor de fundo do jogo
fundo :: Color
fundo = black

-- | Frames por segundo do jogo
fr :: Int
fr = 60

-- | Função auxiliar que carrega as imagens necessárias para o jogo
upLoadImages :: ImmutableTowers -> IO ImmutableTowers
upLoadImages im = do
  background <- loadBMP "imagens/menu.bmp"
  backgroundMenu <- loadBMP "imagens/backgroundMenu.bmp"
  relva <- loadBMP "imagens/terreno/Relva.bmp"
  terra <- loadBMP "imagens/terreno/Terra.bmp"
  agua <- loadBMP "imagens/terreno/Agua.bmp"
  ponte1 <- loadBMP "imagens/terreno/Ponte1.bmp"
  ponte2 <- loadBMP "imagens/terreno/Ponte2.bmp"
  base <- loadBMP "imagens/torres/Base.bmp"
  portal <- loadBMP "imagens/torres/Portal.bmp"
  pause <- loadBMP "imagens/BotaoPausa.bmp"  
  inimigoERoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeERoxo.bmp"
  inimigoNRoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeNRoxo.bmp"
  inimigoSRoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeSRoxo.bmp"
  inimigoORoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeORoxo.bmp"
  inimigoEAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeEAzul.bmp"
  inimigoNAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeNAzul.bmp"
  inimigoSAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeSAzul.bmp"
  inimigoOAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeOAzul.bmp"
  inimigoEVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeEVerde.bmp"
  inimigoNVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeNVerde.bmp"
  inimigoSVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeSVerde.bmp"
  inimigoOVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeOVerde.bmp"
  botaoLoja <- loadBMP "imagens/BotaoLoja.bmp"
  botaoVelocidade <- loadBMP "imagens/botaoVelocidade.bmp"
  return  (im { imagens = [("relva",relva),("terra",terra),("agua",agua),("menu", background),("backgroundMenu", backgroundMenu),("ponte1",ponte1),("ponte2",ponte2)
  ,("inimigoERoxo",inimigoERoxo),("inimigoNRoxo",inimigoNRoxo),("inimigoSRoxo",inimigoSRoxo),("inimigoORoxo",inimigoORoxo),("inimigoEAzul",inimigoEAzul),("inimigoNAzul",inimigoNAzul),("inimigoSAzul",inimigoSAzul),("inimigoOAzul",inimigoOAzul),("inimigoEVerde",inimigoEVerde),("inimigoNVerde",inimigoNVerde),("inimigoSVerde",inimigoSVerde),("inimigoOVerde",inimigoOVerde),("botaoLoja",botaoLoja)] })


-- | Função principal que invoca o jogo
main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  background <- loadBMP "imagens/menu.bmp"
  backgroundMenu <- loadBMP "imagens/backgroundMenu.bmp"
  relva <- loadBMP "imagens/terreno/Relva.bmp"
  terra <- loadBMP "imagens/terreno/Terra.bmp"
  agua <- loadBMP "imagens/terreno/Agua.bmp"
  ponte1 <- loadBMP "imagens/terreno/Ponte1.bmp"
  ponte2 <- loadBMP "imagens/terreno/Ponte2.bmp"
  base <- loadBMP "imagens/torres/Base.bmp"
  portal <- loadBMP "imagens/torres/Portal.bmp"
  pause <- loadBMP "imagens/BotaoPausa.bmp"
  inimigoERoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeERoxo.bmp"
  inimigoNRoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeNRoxo.bmp"
  inimigoSRoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeSRoxo.bmp"
  inimigoORoxo <- loadBMP "imagens/inimigos/SlimeRoxo/SlimeORoxo.bmp"
  inimigoEAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeEAzul.bmp"
  inimigoNAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeNAzul.bmp"
  inimigoSAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeSAzul.bmp"
  inimigoOAzul <- loadBMP "imagens/inimigos/SlimeAzul/SlimeOAzul.bmp"
  inimigoEVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeEVerde.bmp"
  inimigoNVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeNVerde.bmp"
  inimigoSVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeSVerde.bmp"
  inimigoOVerde <- loadBMP "imagens/inimigos/SlimeVerde/SlimeOVerde.bmp"
  botaoLoja <- loadBMP "imagens/BotaoLoja.bmp"
  torrefogo <- loadBMP "imagens/torres/torrefogo.bmp"
  torregelo <- loadBMP "imagens/torres/torregelo.bmp"
  torreresina <- loadBMP "imagens/torres/torreresina.bmp"
  botaoVender <- loadBMP "imagens/BotaoVender.bmp"
  botaoVelocidade <- loadBMP "imagens/botaoVelocidade.bmp"

  play janela 
       fundo 
       fr 
       (it [relva,terra,agua,background,backgroundMenu,ponte1,ponte2,base,portal,pause,inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde,botaoLoja, torrefogo, torregelo, torreresina,botaoVender,botaoVelocidade]) 
       (draw [relva,terra,agua,background,backgroundMenu,ponte1,ponte2,portal,base,pause,inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde,botaoLoja, torrefogo, torregelo, torreresina,botaoVender,botaoVelocidade]) 
       reageEventos 
       reageTempo
  where
    it [relva,terra,agua,background, backgroundMenu,ponte1,ponte2,portal,base,pause,inimigoEAzul,inimigoNAzul,inimigoSAzul,inimigoOAzul,inimigoERoxo,inimigoNRoxo,inimigoSRoxo,inimigoORoxo,inimigoEVerde,inimigoNVerde,inimigoSVerde,inimigoOVerde,botaoLoja, torrefogo, torregelo,torreresina,botaoVender, botaoVelocidade] = ImmutableTowers {
      estadoMenu = MenuInicial,
      jogo = jogoInicial,
      imagens = [("relva",relva),("terra",terra),("agua",agua),("menu", background), ("backgroundMenu", backgroundMenu),("ponte1",ponte1),("ponte2",ponte2),("base",base),("portal",portal),("pause",pause),("inimigoERoxo",inimigoERoxo),("inimigoNRoxo",inimigoNRoxo),("inimigoSRoxo",inimigoSRoxo),("inimigoORoxo",inimigoORoxo),("inimigoEAzul",inimigoEAzul),("inimigoNAzul",inimigoNAzul),("inimigoSAzul",inimigoSAzul),("inimigoOAzul",inimigoOAzul),("inimigoEVerde",inimigoEVerde),("inimigoNVerde",inimigoNVerde),("inimigoSVerde",inimigoSVerde),("inimigoOVerde",inimigoOVerde),("botaoLoja",botaoLoja),("torrefogo",torrefogo),("torregelo",torregelo),("torreresina",torreresina), ("botaoVender",botaoVender), ("botaoVelocidade",botaoVelocidade)],
      novaTorre = Nothing,
      velocidadeJogo = 1
    }
    it _ = error "Unexpected number of images"


