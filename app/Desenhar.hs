{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Parenthesize unary negation" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa1
import Tarefa3
import Eventos
import Tarefa2

desenha :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> 
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
           Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> ImmutableTowers -> Picture
desenha imagemBackgroundNatural imagemBackgroundFlamejante imagemBackgroundCristalina imagemBackgroundFlamejanteDiminuida 
        imagemBackgroundCristalinaDiminuida imagemLojoJogo texturaRelva texturaTerra texturaAgua texturaLava texturaLagrimasCristalinas 
        imagemBase imagemPortal imagemCarvalhoEnt imagemArqueiroFlamejante imagemSabioCorruptor imagemMaquinaDeResina 
        imagemMagoDeGelo imagem1 imagem2 imagem3 imagemFornalhaArdente imagemCanhao imagemJuizGelado imagemGharzul 
        imagemBackgroundFlamejanteDiminuidaCinzenta imagemBackgroundCristalinaDiminuidaCinzenta imagemZalrok imagemZalrokReforcado 
        imagemFenrir imagemTharok imagemAranthisMortifera imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma 
        imagemReaper imagemIgnaroth imagemDragaoCristalino imagemLeaoCristalino imagemFungalith imagemAethralith texturaOuro 
        texturaVida inimigoDerrotado placaMadeira tabelaMadeira numero1 numero2 numero3 numero4 numero5 numero6 numero7 numero8 
        pFogo pGelo pResina pFraqueza pFogoCinzenta pGeloCinzenta pFraquezaCinzenta texturaOuroCinzenta imagemMagoDeGeloCinzenta 
        imagemFornalhaArdenteCinzenta imagemCanhaoCinzenta imagemJuizGeladoCinzenta texturaRocha texturaCinzas barraVidaDeBoss 
        ponteAgua ponteLagrimasCristalinas imagemArvore imagemArvoreMorta imagemCristalAzul imagemCristalMagenta imagemDano 
        imagemAlcance imagemRajada imagemVelocidadeDeAtaque imagemProibido imagemVelocidade imagemVitoria imagemDerrota
        imagemHelpNatural imagemHelpFlamejante imagemHelpCristalino tRE3 tRE6 tRE9 tRE12 tRE15 tRE18 tRE21 tTE3 tTE6 tTE9 tTE12 
        tTE15 tTE18 tTE21 tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21 tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105 tLCE3 tLCE6 tLCE9 tLCE12
        tLCE15 tLCE18 tLCE21 tCE3 tCE6 tCE9 tCE12 tCE15 tCE18 tCE21 tPE3 tPE6 tPE9 tPE12 tPE15 tPE18 tPE21 it = case it of
    MenuPrincipal q _ _ tema _ _ -> Pictures [Scale 1.05 1 imagemBackground,Scale 1.5 1.5 (Translate 0 250 imagemLojoJogo),
                                              Translate (-256) (-188) (desenhaBotao corTema),
                                              Translate (-256) (-20) (desenhaBotao corTema),
                                              Translate (-256) 148 (desenhaBotao corTema),
                                              Translate 256 (-188) (desenhaBotao corTema),
                                              Translate 256 (-20) (desenhaBotao corTema),
                                              Translate 256 148 (desenhaBotao corTema),
                                              Translate 98 124 (Color corTema (Scale 0.5 0.5 (Text "Conquistas"))),
                                              Translate (-336) 124 (Color corTema (Scale 0.5 0.5 (Text "Jogar"))),
                                              Translate (-342) (-212) (Color corTema (Scale 0.5 0.5 (Text "Tema"))),
                                              Translate 198 (-212) (Color corTema (Scale 0.5 0.5 (Text "Sair"))),
                                              Translate (-352) (-44) (Color corTema (Scale 0.5 0.5 (Text "Torres"))),
                                              Translate 134 (-44) (Color corTema (Scale 0.5 0.5 (Text "Inimigos"))),
                                              Translate (-183) (-432) (Color black (Scale 0.25 0.25 (Text "Quantidade de Inimigos"))),
                                              Translate x (-476) (Color black (Scale 0.25 0.25 (Text ("Derrotados: " ++ show q)))),
                                              Translate 620 (-432) (Color black (Scale 0.2 0.2 (Text "By Bruno Magalhaes"))),
                                              Translate 620 (-476) (Color black (Scale 0.2 0.2 (Text "And Joao Oliveira"))),
                                              Translate (-850) (-430) (rectangleSolid 40 80),
                                              Translate (-850) (-430) (circle 53.2),
                                              Translate (-850) (-430) (circle 53.3),
                                              Translate (-850) (-430) (circle 53.4),
                                              Translate (-850) (-430) (circle 53.5),
                                              Translate (-850) (-430) imagemH]
                                                where x 
                                                            | q < 10 = -106
                                                            | q < 100 = -114
                                                            | q < 1000 = -124
                                                            | otherwise = -134
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      (imagemBackground,imagemH)
                                                            | tema == Natural = (imagemBackgroundNatural,imagemHelpNatural)
                                                            | tema == Flamejante = (imagemBackgroundFlamejante,imagemHelpFlamejante)
                                                            | otherwise = (imagemBackgroundCristalina,imagemHelpCristalino)
    InfoJogo _ _ _ tema _ _ n -> Pictures [Scale 1.05 1 imagemBackground,
                                           Translate 0 408 (desenhaBotao corTema),Translate (-134) 388 (Color corTema (Scale 0.3 0.3 (Text "Menu Principal"))),
                                           Translate 0 (-244) (desenhaBotao corTema),Translate (-153) (-264) (Color corTema (Scale 0.3 0.3 (Text "Pagina Seguinte"))),
                                           Translate 0 (-412) (desenhaBotao corTema),Translate (-143) (-432) (Color corTema (Scale 0.3 0.3 (Text "Pagina Anterior"))),
                                           Translate 0 80 (Scale 1.35 1.5 tabelaMadeira),Translate 300 230 (Scale 0.3 0.3 (Text ("Pagina " ++ show n))),
                                           Translate x 170 (Scale 0.5 0.5 (Text titulo)),
                                           Translate (-501) 130 (Scale 0.25 0.25 (Text descricao1)),
                                           Translate (-501) 95 (Scale 0.25 0.25 (Text descricao2)),
                                           Translate (-501) 60 (Scale 0.25 0.25 (Text descricao3)),
                                           Translate (-501) 25 (Scale 0.25 0.25 (Text descricao4)),
                                           Translate (-501) (-10) (Scale 0.25 0.25 (Text descricao5)),
                                           Translate (-501) (-45) (Scale 0.25 0.25 (Text descricao6))]
                                                where imagemBackground
                                                            | tema == Natural = imagemBackgroundNatural
                                                            | tema == Flamejante = imagemBackgroundFlamejante
                                                            | otherwise = imagemBackgroundCristalina
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      (x,titulo,descricao1,descricao2,descricao3,descricao4,descricao5,descricao6)
                                                            | n == 1 = (-274,"Objetivo do Jogo",
                                                                             "O objetivo principal do jogo e contruir torres para evitar",
                                                                             "que os inimigos alcancem a base. Para tal, o jogador deve",
                                                                             "investir ouro para comprar torres que atacam os inimigos",
                                                                             "de modo a derrota-los antes que estes cheguem a base.",
                                                                             "",
                                                                             "")
                                                            | n == 2 = (-160,"O Terreno",
                                                                             "Existem 7 tipos diferentes de terreno: terra, relva, agua,",
                                                                             "lava, pedregulhos, cinzas e lagrimas cristalinas. Terra e",
                                                                             "pedregulhos constituem os caminhos pnde se situam os",
                                                                             "portais, os inimigos e a base; relva e cinzas sao os locais",
                                                                             "onde e possivel contruir torres; lagrimas cristalinas",
                                                                             "regeneram inimigos proximos.")
                                                            | n == 3 = (-116,"A Base",
                                                                             "Estrutura imovel situada em terra ou pedregulhos que",
                                                                             "possui um valor de vida (inicialmente 100) que diminui",
                                                                             "sempre que um inimigo alcanca a base. O jogo e",
                                                                             "considerado ganho quando todos os inimigos sao derrotados",
                                                                             "ou perdido quando a vida da base atinge 0.",
                                                                             "")
                                                            | n == 4 = (-164,"Os Portais",
                                                                             "Estruturas imoveis situadas em terra ou pedregulhos que",
                                                                             "sao responsaveis por introduzir os inimigos no jogo (onde",
                                                                             "se situam). Cada portal possui uma quantidade definida de",
                                                                             "ondas que avancam ao longo do jogo.",
                                                                             "",
                                                                             "")
                                                            | n == 5 = (-194,"Os Projeteis",
                                                                             "Existem 4 tipos diferentes de projeteis: fogo, gelo, resina e",
                                                                             "fraqueza. Quando um inimigo e atingido por um projetil de",
                                                                             "dado tipo, este e afetado pelos seus respetivos efeitos",
                                                                             "(assumindo que nao e imune a projeteis desse tipo).",
                                                                             "",
                                                                             "")
                                                            | n == 6 = (-274,"Projetil de Fogo",
                                                                             "Inimigos afetados pelo efeito de fogo perdem 10 de vida",
                                                                             "por segundo. Fogo cancela o gelo, pelo que um",
                                                                             "inimigo nao pode ser afetado por ambos em simultaneo.",
                                                                             "",
                                                                             "",
                                                                             "")
                                                            | n == 7 = (-250,"Projetil de Gelo",
                                                                             "Inimigos afetados pelo efeito do gelo ficam completamente",
                                                                             "imobilizados. Gelo cancela o fogo, pelo que um inimigo nao",
                                                                             "pode ser afetado por ambos em simultaneo.",
                                                                             "",
                                                                             "",
                                                                             "")
                                                            | n == 8 = (-288,"Projetil de Resina",
                                                                             "Inimigos afetados pelo efeito da resina movem-se 30%",
                                                                             "mais lentamente. Fogo queima (elimina) a resina, dobrando",
                                                                             "a sua duracao, pelo que um inimigo nao pode ser afetado",
                                                                             "por ambos em simultaneo.",
                                                                             "",
                                                                             "")
                                                            | n == 9 = (-324,"Projetil de Fraqueza",
                                                                             "Inimigos afetados pelo efeito de fraqueza levam o dobro do",
                                                                             "dano direto das torres (nao afetando o fogo). Este efeito e",
                                                                             "essencial para lidar com inimigos com uma elevada",
                                                                             "quantidade de vida.",
                                                                             "",
                                                                             "")
                                                            | n == 10 = (-186,"Os Inimigos",
                                                                              "Existem 5 fatores que caracterizam os inimigos: a vida, o",
                                                                              "dano, a velocidade, o butim e as imunidades. O dano",
                                                                              "indica quanta vida a base perde quando o inimigo a",
                                                                              "atinge; o butim e a quantidade de ouro que o jogador",
                                                                              "ganha ao derrotar o inimigo; as imunidades sao os tipos",
                                                                              "de projeteis que nao afetam o inimigo.")
                                                            | n == 11 = (-154,"As Torres",
                                                                              "Existem 6 fatores que caracterizam as torres: o dano, o",
                                                                              "alcance, a rajada, a velocidade de ataque, o custo e o tipo",
                                                                              "de projetil. A rajada refere-se a quantos inimigos a torre",
                                                                              "pode atingir cum um so ataque; a velocidade de ataque e",
                                                                              "o intervalo de tempo minimo entre ataques.","")
                                                            | otherwise = (-138,"A Chuva",
                                                                                "Ao longo do jogo e possivel a ocorrencia de chuva que",
                                                                                "pode ser normal, de magma ou de lagrimas cristalinas. A",
                                                                                "chuva abranda todos os inimigos em 15% e chuva: normal",
                                                                                "previne o efeito do fogo; de magma previne o efeito do",
                                                                                "gelo e da resina, para alem de queimar os inimigos; de",
                                                                                "lagrimas cristalinas regenera os inimigos.")
    SelecaoNiveis q cs nc tema _ _ -> Pictures [Scale 1.05 1 imagemBackground,Scale 1.5 1.5 (Translate 0 250 imagemLojoJogo),
                                                Scale 1.25 1.25 (Translate (-403) 160 botaoNivel1),
                                                Translate (-677) 188 (Color cor1 (Scale 0.275 0.275 (Text "Planicies Verdejantes"))),
                                                Scale 1.25 1.25 (Translate 404 160 botaoNivel1Elite),
                                                Translate 280 188 (Color cor1Elite (Scale 0.275 0.275 (Text "Planicies Verdejantes Elite"))),
                                                Scale 1.25 1.25 (Translate (-453) 0 botaoNivel2),
                                                Translate (-721) (-13) (Color cor2 (Scale 0.275 0.275 (Text "Floresta da Morte"))),
                                                Scale 1.25 1.25 (Translate 454 0 botaoNivel2Elite),
                                                Translate 364 (-13) (Color cor2Elite (Scale 0.275 0.275 (Text "Floresta da Morte Elite"))),
                                                Scale 1.25 1.25 (Translate (-403) (-159) botaoNivel3),
                                                Translate (-663) (-211) (Color cor3 (Scale 0.275 0.275 (Text "Vulcao das Trevas"))),
                                                Scale 1.25 1.25 (Translate 404 (-159) botaoNivel3Elite),
                                                Translate 300 (-211) (Color cor3Elite (Scale 0.275 0.275 (Text "Vulcao das Trevas Elite"))),
                                                Scale 1.25 1.25 botaoNivelSecreto,
                                                textoNivelSecreto,
                                                Scale 1.25 1.25 botaoNivelSecretoElite,
                                                textoNivelSecretoElite,
                                                Scale 1.25 1.25 (Translate 0 0 (desenhaBotao corTema)),
                                                Translate (-181) (-19) (Color corTema (Scale 0.4 0.4 (Text "Menu Principal")))]
                                                      where y = if q < 1000 || length cs < 5 then 9999 else -319
                                                            botaoNivelSecreto = Translate (-453) y (desenhaBotao corTema)
                                                            botaoNivelSecretoElite = if 4 `elem` nc then Translate 454 y (desenhaBotao corTema) else Translate 454 y (desenhaBotao (greyN 0.5))
                                                            textoNivelSecreto = Translate (-695) (y - 96) (Color corTema (Scale 0.275 0.275 (Text "Ilhas Cristalinas")))
                                                            textoNivelSecretoElite = if 4 `elem` nc then Translate 394 (y - 96) (Color corTema (Scale 0.275 0.275 (Text "Ilhas Cristalinas Elite"))) else Translate 394 (y - 96) (Color (greyN 0.5) (Scale 0.275 0.275 (Text "Ilhas Cristalinas Elite")))
                                                            botaoNivel1 = desenhaBotao corTema
                                                            botaoNivel1Elite = if 1 `elem` nc then desenhaBotao corTema else desenhaBotao (greyN 0.5)
                                                            botaoNivel2 = if 1 `elem` nc then desenhaBotao corTema else desenhaBotao (greyN 0.5)
                                                            botaoNivel2Elite = if 2 `elem` nc then desenhaBotao corTema else desenhaBotao (greyN 0.5)
                                                            botaoNivel3 = if 2 `elem` nc then desenhaBotao corTema else desenhaBotao (greyN 0.5)
                                                            botaoNivel3Elite = if 3 `elem` nc then desenhaBotao corTema else desenhaBotao (greyN 0.5)
                                                            cor1 = corTema
                                                            cor1Elite = if 1 `elem` nc then corTema else greyN 0.5
                                                            cor2 = if 1 `elem` nc then corTema else greyN 0.5
                                                            cor2Elite = if 2 `elem` nc then corTema else greyN 0.5
                                                            cor3 = if 2 `elem` nc then corTema else greyN 0.5
                                                            cor3Elite = if 3 `elem` nc then corTema else greyN 0.5
                                                            corTema
                                                                  | tema == Natural = verdeMedio
                                                                  | tema == Flamejante = red
                                                                  | otherwise = dark magenta
                                                            imagemBackground
                                                                  | tema == Natural = imagemBackgroundNatural
                                                                  | tema == Flamejante = imagemBackgroundFlamejante
                                                                  | otherwise = imagemBackgroundCristalina
    InfoTorres _ _ _ tema _ j _ -> Pictures [Scale 1.05 1 imagemBackground,
                                             Translate (-319) 0 (Color corTema (rectangleSolid 12 1080)),
                                             Translate 320 0 (Color corTema (rectangleSolid 12 1080)),
                                             Translate 0 180 (Color corTema (rectangleSolid 1920 12)),
                                             Translate 0 (-179) (Color corTema (rectangleSolid 1920 12)),
                                             Translate (-319) 0 (Color black (rectangleSolid 8 1080)),
                                             Translate 320 0 (Color black (rectangleSolid 8 1080)),
                                             Translate 0 180 (Color black (rectangleSolid 1920 8)),
                                             Translate 0 (-179) (Color black (rectangleSolid 1920 8)),
                                             desenhaBotao corTema,Translate (-181 + x) (-19) (Color corTema (Scale 0.4 0.4 (Text textoBotao))),
                                             Translate (-160) 340 (Scale 0.4 0.4 placaMadeira),
                                             Translate (-160) (-349) (Scale 0.4 0.4 placaMadeira),
                                             Translate 470 0 (Scale 0.4 0.4 placaMadeira),
                                             Translate (-790) 0 (Scale 0.4 0.4 placaMadeira),
                                             Translate (-790) 340 (Scale 0.4 0.4 placaMadeira),
                                             Translate 470 340 (Scale 0.4 0.4 placaMadeira),
                                             Translate (-790) (-349) (Scale 0.4 0.4 placaMadeira),
                                             Translate 470 (-349) (Scale 0.4 0.4 placaMadeira),
                                             Translate 106 340 (Scale 0.35 0.75 tabelaMadeira),
                                             Translate 106 (-349) (Scale 0.35 0.75 tabelaMadeira),
                                             Translate 736 0 (Scale 0.35 0.75 tabelaMadeira),
                                             Translate (-524) 0 (Scale 0.35 0.75 tabelaMadeira),
                                             Translate (-524) 340 (Scale 0.35 0.75 tabelaMadeira),
                                             Translate 736 340 (Scale 0.35 0.75 tabelaMadeira),
                                             Translate (-524) (-349) (Scale 0.35 0.75 tabelaMadeira),
                                             Translate 736 (-349) (Scale 0.35 0.75 tabelaMadeira),
                                             Translate (-790) 340 (Scale 1.6 1.6 imagemArqueiroFlamejante),
                                             Translate (-638) 390 (Scale 0.7 0.7 imagemDano),
                                             Translate (-628) 380 (Scale 0.12 0.12 (Text "16/24/32")),
                                             Translate (-638) 290 (Scale 0.7 0.7 imagemRajada),
                                             Translate (-618) 280 (Scale 0.12 0.12 (Text "1/1/1")),
                                             Translate (-500) 390 (Scale 0.7 0.7 imagemAlcance),
                                             Translate (-480) 380 (Scale 0.12 0.12 (Text "2/2.5/3")),
                                             Translate (-500) 290 (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate (-480) 280 (Scale 0.12 0.12 (Text "1.2/1/0.8")),
                                             Translate (-812) 260 (Scale 0.2 0.2 (Text (show (fst arqueiroFlamejante)))),
                                             Translate (-726) 404 (Scale 0.07 0.07 numero1), 
                                             Translate (-854) 404 (Scale 0.1 0.1 pFogo),
                                             Translate (-854) 276 (Scale 0.15 0.15 texturaOuro),
                                             Translate (-160) 340 (Scale 1.6 1.6 imagemSabioCorruptor),
                                             Translate (-8) 390 (Scale 0.7 0.7 imagemDano),
                                             Translate 2 380 (Scale 0.12 0.12 (Text "20/40/60")),
                                             Translate (-8) 290 (Scale 0.7 0.7 imagemRajada),
                                             Translate 12 280 (Scale 0.12 0.12 (Text "1/1/1")),
                                             Translate 130 390 (Scale 0.7 0.7 imagemAlcance),
                                             Translate 150 380 (Scale 0.12 0.12 (Text "2.5/3/3.5")),
                                             Translate 130 290 (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate 150 280 (Scale 0.12 0.12 (Text "4/3.5/3")),
                                             Translate (-182) 260 (Scale 0.2 0.2 (Text (show (fst sabioCorruptor)))),
                                             Translate (-96) 404 (Scale 0.07 0.07 numero2),
                                             Translate (-224) 404 (Scale 0.1 0.1 pFraqueza),
                                             Translate (-224) 276 (Scale 0.15 0.15 texturaOuro),
                                             Translate 470 340 (Scale 1.6 1.6 imagemCarvalhoEnt),
                                             Translate 622 390 (Scale 0.7 0.7 imagemDano),
                                             Translate 632 380 (Scale 0.12 0.12 (Text "5/7.5/10")),
                                             Translate 622 290 (Scale 0.7 0.7 imagemRajada),
                                             Translate 642 280 (Scale 0.12 0.12 (Text "Infinito")),
                                             Translate 760 390 (Scale 0.7 0.7 imagemAlcance),
                                             Translate 780 380 (Scale 0.12 0.12 (Text "1.5/1.75/2")),
                                             Translate 760 290 (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate 780 280 (Scale 0.12 0.12 (Text "0.5/0.45/0.4")),
                                             Translate 448 260 (Scale 0.2 0.2 (Text (show (fst carvalhoEnt)))),
                                             Translate 534 404 (Scale 0.07 0.07 numero3),
                                             Translate 406 404 (Scale 0.1 0.1 pResina),
                                             Translate 406 276 (Scale 0.15 0.15 texturaOuro),
                                             Translate (-790) 0 (Scale 1.6 1.6 imagemMaquinaDeResina),
                                             Translate (-638) 50 (Scale 0.7 0.7 imagemDano),
                                             Translate (-628) 40 (Scale 0.12 0.12 (Text "20/25/30")),
                                             Translate (-638) (-50) (Scale 0.7 0.7 imagemRajada),
                                             Translate (-618) (-60) (Scale 0.12 0.12 (Text "1/2/3")),
                                             Translate (-500) 50 (Scale 0.7 0.7 imagemAlcance),
                                             Translate (-480) 40 (Scale 0.12 0.12 (Text "2/2.5/3")),
                                             Translate (-500) (-50) (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate (-480) (-60) (Scale 0.12 0.12 (Text "2/1.6/1.2")),
                                             Translate (-812) (-80) (Scale 0.2 0.2 (Text (show (fst maquinaDeResina)))),
                                             Translate (-726) 64 (Scale 0.07 0.07 numero4),
                                             Translate (-854) 64 (Scale 0.1 0.1 pResina),
                                             Translate (-854) (-64) (Scale 0.15 0.15 texturaOuro),
                                             Translate 470 0 (Scale 1.6 1.6 imagemMagoDeGelo),
                                             Translate 622 50 (Scale 0.7 0.7 imagemDano),
                                             Translate 632 40 (Scale 0.12 0.12 (Text "8/12/16")),
                                             Translate 622 (-50) (Scale 0.7 0.7 imagemRajada),
                                             Translate 642 (-60) (Scale 0.12 0.12 (Text "1/1/1")),
                                             Translate 760 50 (Scale 0.7 0.7 imagemAlcance),
                                             Translate 780 40 (Scale 0.12 0.12 (Text "2/2.25/2.5")),
                                             Translate 760 (-50) (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate 780 (-60) (Scale 0.12 0.12 (Text "2/1.9/1.8")),
                                             Translate 452 (-80) (Scale 0.2 0.2 (Text (show (fst magoDeGelo)))),
                                             Translate 534 64 (Scale 0.07 0.07 numero5),
                                             Translate 406 64 (Scale 0.1 0.1 pGelo),
                                             Translate 406 (-64) (Scale 0.15 0.15 texturaOuro),
                                             Translate (-790) (-349) (Scale 1.6 1.6 imagemFornalhaArdente),
                                             Translate (-638) (-299) (Scale 0.7 0.7 imagemDano),
                                             Translate (-618) (-309) (Scale 0.12 0.12 (Text "20/40/60")),
                                             Translate (-638) (-399) (Scale 0.7 0.7 imagemRajada),
                                             Translate (-618) (-409) (Scale 0.12 0.12 (Text "Infinito")),
                                             Translate (-500) (-299) (Scale 0.7 0.7 imagemAlcance),
                                             Translate (-480) (-309) (Scale 0.12 0.12 (Text "2/2.5/3")),
                                             Translate (-500) (-399) (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate (-480) (-409) (Scale 0.12 0.12 (Text "6/5.25/4.5")),
                                             Translate (-812) (-429) (Scale 0.2 0.2 (Text (show (fst fornalhaArdente)))),
                                             Translate (-736) (-285) (Scale 0.07 0.07 numero6),
                                             Translate (-854) (-285) (Scale 0.1 0.1 pFogo),
                                             Translate (-854) (-415) (Scale 0.15 0.15 texturaOuro),
                                             Translate (-160) (-349) (Scale 1.6 1.6 imagemCanhao),
                                             Translate (-8) (-299) (Scale 0.7 0.7 imagemDano),
                                             Translate 2 (-309) (Scale 0.12 0.12 (Text "100/150/200")),
                                             Translate (-8) (-399) (Scale 0.7 0.7 imagemRajada),
                                             Translate 12 (-409) (Scale 0.12 0.12 (Text "1/1/1")),
                                             Translate 130 (-299) (Scale 0.7 0.7 imagemAlcance),
                                             Translate 150 (-309) (Scale 0.12 0.12 (Text "2/2.5/3")),
                                             Translate 130 (-399) (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate 150 (-409) (Scale 0.12 0.12 (Text "3.5/3/2.5")),
                                             Translate (-182) (-429) (Scale 0.2 0.2 (Text (show (fst canhao)))),
                                             Translate (-96) (-285) (Scale 0.07 0.07 numero7),
                                             Translate (-224) (-285) (Scale 0.1 0.1 pFraqueza),
                                             Translate (-224) (-415) (Scale 0.15 0.15 texturaOuro),
                                             Translate 470 (-349) (Scale 1.6 1.6 imagemJuizGelado),
                                             Translate 622 (-299) (Scale 0.7 0.7 imagemDano),
                                             Translate 632 (-309) (Scale 0.12 0.12 (Text "150/225/300")),
                                             Translate 622 (-399) (Scale 0.7 0.7 imagemRajada),
                                             Translate 642 (-409) (Scale 0.12 0.12 (Text "1/1/1")),
                                             Translate 760 (-299) (Scale 0.7 0.7 imagemAlcance),
                                             Translate 780 (-309) (Scale 0.12 0.12 (Text "1.5/1.75/2")),
                                             Translate 760 (-399) (Scale 0.7 0.7 imagemVelocidadeDeAtaque),
                                             Translate 780 (-409) (Scale 0.12 0.12 (Text "4/3.5/3")),
                                             Translate 448 (-429) (Scale 0.2 0.2 (Text (show (fst juizGelado)))),
                                             Translate 536 (-285) (Scale 0.07 0.07 numero8),
                                             Translate 406 (-285) (Scale 0.1 0.1 pGelo),
                                             Translate 406 (-415) (Scale 0.15 0.15 texturaOuro)]
                                                where corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      imagemBackground
                                                            | tema == Natural = imagemBackgroundNatural
                                                            | tema == Flamejante = imagemBackgroundFlamejante
                                                            | otherwise = imagemBackgroundCristalina
                                                      (x,textoBotao) = case j of
                                                            Nothing -> (0,"Menu Principal")
                                                            _ -> (84,"Retomar")
    InfoInimigos _ _ _ tema _ j n _ -> Pictures [Scale 1.05 1 imagemBackground,
                                                 Translate (-319) 0 (Color corTema (rectangleSolid 12 1080)),
                                                 Translate 320 0 (Color corTema (rectangleSolid 12 1080)),
                                                 Translate 0 180 (Color corTema (rectangleSolid 1920 12)),
                                                 Translate 0 (-179) (Color corTema (rectangleSolid 1920 12)),
                                                 Translate (-319) 0 (Color black (rectangleSolid 8 1080)),
                                                 Translate 320 0 (Color black (rectangleSolid 8 1080)),
                                                 Translate 0 180 (Color black (rectangleSolid 1920 8)),
                                                 Translate 0 (-179) (Color black (rectangleSolid 1920 8)),
                                                 Translate 0 80 (desenhaBotao corTema),Translate (-137 + x) 62 (Color corTema (Scale 0.3 0.3 (Text textoBotao1))),
                                                 Translate 0 (-80) (desenhaBotao corTema), Translate (-153 + x') (-99) (Color corTema (Scale 0.3 0.3 (Text textoBotao2))),
                                                 Translate (-160) 340 (Scale 0.4 0.4 placaMadeira),
                                                 Translate (-160) (-349) (Scale 0.4 0.4 placaMadeira),
                                                 Translate 470 0 (Scale 0.4 0.4 placaMadeira),
                                                 Translate (-790) 0 (Scale 0.4 0.4 placaMadeira),
                                                 Translate (-790) 340 (Scale 0.4 0.4 placaMadeira),
                                                 Translate 470 340 (Scale 0.4 0.4 placaMadeira),
                                                 Translate (-790) (-349) (Scale 0.4 0.4 placaMadeira),
                                                 Translate 470 (-349) (Scale 0.4 0.4 placaMadeira),
                                                 Translate 106 340 (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate 106 (-349) (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate 736 0 (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate (-524) 0 (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate (-524) 340 (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate 736 340 (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate (-524) (-349) (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate 736 (-349) (Scale 0.35 0.75 tabelaMadeira),
                                                 Translate (-794) 340 (Scale 3 3 inimigo1),
                                                 Translate (-613) 380 (Scale 0.12 0.12 (Text vida1)),
                                                 Translate (-613) 280 (Scale 0.12 0.12 (Text butim1)),
                                                 Translate (-475) 380 (Scale 0.12 0.12 (Text dano1)),
                                                 Translate (-475) 280 (Scale 0.12 0.12 (Text velocidade1)),
                                                 Translate (-638) 390 (Scale 0.2 0.2 texturaVida),
                                                 Translate (-638) 290 (Scale 0.2 0.2 texturaOuro),
                                                 Translate (-500) 390 (Scale 0.7 0.7 imagemDano),
                                                 Translate (-500) 290 (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate (-854) 404 (Scale 0.1 0.1 pFogo),
                                                 Translate (-726) 404 (Scale 0.1 0.1 pGelo),
                                                 Translate (-854) 276 (Scale 0.1 0.1 pResina),
                                                 Translate (-726) 276 (Scale 0.1 0.1 pFraqueza),
                                                 Translate (-162) 340 (Scale 3 3 inimigo2),
                                                 Translate 17 380 (Scale 0.12 0.12 (Text vida2)),
                                                 Translate 17 280 (Scale 0.12 0.12 (Text butim2)),
                                                 Translate 155 380 (Scale 0.12 0.12 (Text dano2)),
                                                 Translate 155 280 (Scale 0.12 0.12 (Text velocidade2)),
                                                 Translate (-8) 390 (Scale 0.2 0.2 texturaVida),
                                                 Translate (-8) 290 (Scale 0.2 0.2 texturaOuro),
                                                 Translate 130 390 (Scale 0.7 0.7 imagemDano),
                                                 Translate 130 290 (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate (-224) 404 (Scale 0.1 0.1 pFogo),
                                                 Translate (-96) 404 (Scale 0.1 0.1 pGelo),
                                                 Translate (-224) 276 (Scale 0.1 0.1 pResina),
                                                 Translate (-96) 276 (Scale 0.1 0.1 pFraqueza),
                                                 Translate 470 340 (Scale 3 3 inimigo3),
                                                 Translate 647 380 (Scale 0.12 0.12 (Text vida3)),
                                                 Translate 647 280 (Scale 0.12 0.12 (Text butim3)),
                                                 Translate 785 380 (Scale 0.12 0.12 (Text dano3)),
                                                 Translate 785 280 (Scale 0.12 0.12 (Text velocidade3)),
                                                 Translate 622 390 (Scale 0.2 0.2 texturaVida),
                                                 Translate 622 290 (Scale 0.2 0.2 texturaOuro),
                                                 Translate 760 390 (Scale 0.7 0.7 imagemDano),
                                                 Translate 760 290 (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate 406 404 (Scale 0.1 0.1 pFogo),
                                                 Translate 534 404 (Scale 0.1 0.1 pGelo),
                                                 Translate 406 276 (Scale 0.1 0.1 pResina),
                                                 Translate 534 276 (Scale 0.1 0.1 pFraqueza),
                                                 Translate (-790) 0 (Scale 3 3 inimigo4),
                                                 Translate (-613) 40 (Scale 0.12 0.12 (Text vida4)),
                                                 Translate (-613) (-60) (Scale 0.12 0.12 (Text butim4)),
                                                 Translate (-475) 40 (Scale 0.12 0.12 (Text dano4)),
                                                 Translate (-475) (-60) (Scale 0.12 0.12 (Text velocidade4)),
                                                 Translate (-638) 50 (Scale 0.2 0.2 texturaVida),
                                                 Translate (-638) (-50) (Scale 0.2 0.2 texturaOuro),
                                                 Translate (-500) 50 (Scale 0.7 0.7 imagemDano),
                                                 Translate (-500) (-50) (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate (-854) 64 (Scale 0.1 0.1 pFogo),
                                                 Translate (-726) 64 (Scale 0.1 0.1 pGelo),
                                                 Translate (-854) (-64) (Scale 0.1 0.1 pResina),
                                                 Translate (-726) (-64) (Scale 0.1 0.1 pFraqueza),
                                                 Translate 470 0 (Scale 3 3 inimigo5),
                                                 Translate 647 40 (Scale 0.12 0.12 (Text vida5)),
                                                 Translate 647 (-60) (Scale 0.12 0.12 (Text butim5)),
                                                 Translate 785 40 (Scale 0.12 0.12 (Text dano5)),
                                                 Translate 785 (-60) (Scale 0.12 0.12 (Text velocidade5)),
                                                 Translate 622 50 (Scale 0.2 0.2 texturaVida),
                                                 Translate 622 (-50) (Scale 0.2 0.2 texturaOuro),
                                                 Translate 760 50 (Scale 0.7 0.7 imagemDano),
                                                 Translate 760 (-50) (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate 406 64 (Scale 0.1 0.1 pFogo),
                                                 Translate 534 64 (Scale 0.1 0.1 pGelo),
                                                 Translate 406 (-64) (Scale 0.1 0.1 pResina),
                                                 Translate 534 (-64) (Scale 0.1 0.1 pFraqueza),
                                                 Translate (-790) (-349) (Scale 3 3 inimigo6),
                                                 Translate (-613) (-309) (Scale 0.12 0.12 (Text vida6)),
                                                 Translate (-613) (-409) (Scale 0.12 0.12 (Text butim6)),
                                                 Translate (-475) (-309) (Scale 0.12 0.12 (Text dano6)),
                                                 Translate (-475) (-409) (Scale 0.12 0.12 (Text velocidade6)),
                                                 Translate (-638) (-299) (Scale 0.2 0.2 texturaVida),
                                                 Translate (-638) (-399) (Scale 0.2 0.2 texturaOuro),
                                                 Translate (-500) (-299) (Scale 0.7 0.7 imagemDano),
                                                 Translate (-500) (-399) (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate (-854) (-285) (Scale 0.1 0.1 pFogo),
                                                 Translate (-726) (-285) (Scale 0.1 0.1 pGelo),
                                                 Translate (-854) (-413) (Scale 0.1 0.1 pResina),
                                                 Translate (-726) (-413) (Scale 0.1 0.1 pFraqueza),
                                                 Translate (-160) (-349) (Scale 3 3 inimigo7),
                                                 Translate 17 (-309) (Scale 0.12 0.12 (Text vida7)),
                                                 Translate 17 (-409) (Scale 0.12 0.12 (Text butim7)),
                                                 Translate 155 (-309) (Scale 0.12 0.12 (Text dano7)),
                                                 Translate 155 (-409) (Scale 0.12 0.12 (Text velocidade7)),
                                                 Translate (-8) (-299) (Scale 0.2 0.2 texturaVida),
                                                 Translate (-8) (-399) (Scale 0.2 0.2 texturaOuro),
                                                 Translate 130 (-299) (Scale 0.7 0.7 imagemDano),
                                                 Translate 130 (-399) (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate (-224) (-285) (Scale 0.1 0.1 pFogo),
                                                 Translate (-96) (-285) (Scale 0.1 0.1 pGelo),
                                                 Translate (-224) (-413) (Scale 0.1 0.1 pResina),
                                                 Translate (-96) (-413) (Scale 0.1 0.1 pFraqueza),
                                                 Translate 470 (-349) (Scale 3 3 inimigo8),
                                                 Translate 647 (-309) (Scale 0.12 0.12 (Text vida8)),
                                                 Translate 647 (-409) (Scale 0.12 0.12 (Text butim8)),
                                                 Translate 785 (-309) (Scale 0.12 0.12 (Text dano8)),
                                                 Translate 785 (-409) (Scale 0.12 0.12 (Text velocidade8)),
                                                 Translate 622 (-299) (Scale 0.2 0.2 texturaVida),
                                                 Translate 622 (-399) (Scale 0.2 0.2 texturaOuro),
                                                 Translate 760 (-299) (Scale 0.7 0.7 imagemDano),
                                                 Translate 760 (-399) (Scale 0.7 0.7 imagemVelocidade),
                                                 Translate 406 (-285) (Scale 0.1 0.1 pFogo),
                                                 Translate 534 (-285) (Scale 0.1 0.1 pGelo),
                                                 Translate 406 (-413) (Scale 0.1 0.1 pResina),
                                                 Translate 534 (-413) (Scale 0.1 0.1 pFraqueza),
                                                 desenhaImunidades]
                                                      where corTema
                                                                  | tema == Natural = verdeMedio
                                                                  | tema == Flamejante = red
                                                                  | otherwise = dark magenta
                                                            imagemBackground
                                                                  | tema == Natural = imagemBackgroundNatural
                                                                  | tema == Flamejante = imagemBackgroundFlamejante
                                                                  | otherwise = imagemBackgroundCristalina
                                                            (x,textoBotao1) = case j of
                                                                  Nothing -> (0,"Menu Principal")
                                                                  _ -> (60,"Retomar")
                                                            (x',textoBotao2) = case n of
                                                                  1 -> (0,"Pagina Seguinte")
                                                                  _ -> (10,"Pagina Anterior")
                                                            inimigo1 = if n == 1 then imagemGharzul else imagemOnyc
                                                            inimigo2 = if n == 1 then imagemZalrok else imagemMonstroDeMagma
                                                            inimigo3 = if n == 1 then imagemFenrir else imagemReaper
                                                            inimigo4 = if n == 1 then imagemTharok else imagemIgnaroth
                                                            inimigo5 = if n == 1 then imagemAranthisMortifera else imagemDragaoCristalino
                                                            inimigo6 = if n == 1 then imagemZalrokReforcado else imagemLeaoCristalino
                                                            inimigo7 = if n == 1 then imagemDragaoDeRocha else imagemFungalith
                                                            inimigo8 = if n == 1 then imagemFenrirSulfr else imagemAethralith
                                                            desenhaImunidades
                                                                  | n == 1 = Pictures [Translate 534 404 imagemProibido,
                                                                                       Translate 406 276 imagemProibido,
                                                                                       Translate (-726) 64 imagemProibido,
                                                                                       Translate 534 (-64) imagemProibido,
                                                                                       Translate 534 (-285) imagemProibido,
                                                                                       Translate 406 (-413) imagemProibido]
                                                                  | n == 2 = Pictures [Translate (-224) 404 imagemProibido,
                                                                                       Translate 534 276 imagemProibido,
                                                                                       Translate (-854) 64 imagemProibido,
                                                                                       Translate (-726) 64 imagemProibido,
                                                                                       Translate (-854) (-413) imagemProibido,
                                                                                       Translate 534 (-285) imagemProibido]
                                                            (vida1,dano1,butim1,velocidade1) = if n == 1 then ("50/60","10","10","0.5") else ("150/180","20","30","1")
                                                            (vida2,dano2,butim2,velocidade2) = if n == 1 then ("150/180","20","20","0.4") else ("2400/2880","40","80","0.35")
                                                            (vida3,dano3,butim3,velocidade3) = if n == 1 then ("75/90","10","25","0.8") else ("900/1180","30","75","0.5")
                                                            (vida4,dano4,butim4,velocidade4) = if n == 1 then ("600/720","100","100","0.5") else ("20000/24000","100","600","0.35")
                                                            (vida5,dano5,butim5,velocidade5) = if n == 1 then ("150/180","20","20","0.5") else ("350/420","30","30","0.7")
                                                            (vida6,dano6,butim6,velocidade6) = if n == 1 then ("320/384","30","40","0.4") else ("200/240","30","30","0.8")
                                                            (vida7,dano7,butim7,velocidade7) = if n == 1 then ("700/840","40","80","0.5") else ("2500/3000","100","200","0.4")
                                                            (vida8,dano8,butim8,velocidade8) = if n == 1 then ("1200/1440","100","100","0.7") else ("8000/9600","100","600","0.4")
    Temas _ _ nc tema _ _ -> Pictures [Scale 1.05 1 imagemBackgroundNatural,
                                       Scale 1.05 1 backgroundflamejante,
                                       Translate 631 0 (Scale 1.05 1 backgroundcristalina),
                                       Translate 0 408 (desenhaBotao corTema),Translate (-181) 388 (Color corTema (Scale 0.4 0.4 (Text "Menu Principal"))),
                                       Translate 0 (-409) botaoLaranja,Translate (-131) (-431) (Color corLaranja (Scale 0.4 0.4 (Text "Flamejante"))),
                                       Translate (-639) (-409) botaoVerde,Translate (-719) (-431) (Color verdeMedio (Scale 0.4 0.4 (Text "Natural"))),
                                       Translate 640 (-409) botaoRosa,Translate 540 (-431) (Color corRosa (Scale 0.4 0.4 (Text "Cristalino"))),
                                       Translate (-319) 0 (Color corTema (rectangleSolid 12 1080)),Translate (-319) 0 (Color black (rectangleSolid 8 1080)),
                                       Translate 320 0 (Color corTema (rectangleSolid 12 1080)),Translate 320 0 (Color black (rectangleSolid 8 1080))]
                                          where botaoVerde = desenhaBotao verdeMedio
                                                botaoLaranja = if 3 `elem` nc then desenhaBotao red else desenhaBotao (greyN 0.5)
                                                botaoRosa = if 4 `elem` nc then desenhaBotao magenta else desenhaBotao (greyN 0.5)
                                                corLaranja = if 3 `elem` nc then red else greyN 0.5
                                                corRosa = if 4 `elem` nc then magenta else greyN 0.5
                                                corTema
                                                      | tema == Natural = verdeMedio
                                                      | tema == Flamejante = red
                                                      | otherwise = dark magenta
                                                backgroundflamejante
                                                      | 3 `elem` nc = imagemBackgroundFlamejanteDiminuida
                                                      | otherwise = imagemBackgroundFlamejanteDiminuidaCinzenta
                                                backgroundcristalina
                                                      | 4 `elem` nc = imagemBackgroundCristalinaDiminuida
                                                      | otherwise = imagemBackgroundCristalinaDiminuidaCinzenta
    Conquistas q cs _ tema _ _ -> Pictures [Scale 1.05 1 imagemBackground,
                                            Translate (-319) 0 (Color corTema (rectangleSolid 12 1080)),
                                            Translate 320 0 (Color corTema (rectangleSolid 12 1080)),
                                            Translate 0 180 (Color corTema (rectangleSolid 1920 12)),
                                            Translate 0 (-179) (Color corTema (rectangleSolid 1920 12)),
                                            Translate (-319) 0 (Color black (rectangleSolid 8 1080)),
                                            Translate 320 0 (Color black (rectangleSolid 8 1080)),
                                            Translate 0 180 (Color black (rectangleSolid 1920 8)),
                                            Translate 0 (-179) (Color black (rectangleSolid 1920 8)),
                                            desenhaBotao corTema,Translate (-181) (-19) (Color corTema (Scale 0.4 0.4 (Text "Menu Principal"))),
                                            Translate 0 340 (desenhaBotao cor2),
                                            Translate 0 (-349) (desenhaBotao cor7),
                                            Translate 630 0 (desenhaBotao cor5),
                                            Translate (-629) 0 (desenhaBotao cor4),
                                            Translate (-629) 340 (desenhaBotao cor1),
                                            Translate 630 340 (desenhaBotao cor3),
                                            Translate (-629) (-349) (desenhaBotao cor6),
                                            Translate 630 (-349) (desenhaBotao cor8),
                                            Translate (-731) 334 (Color cor1 (Scale 0.16 0.16 (Text "Derrote 10 inimigos"))),
                                            Translate (-109) 334 (Color cor2 (Scale 0.16 0.16 (Text "Derrote 100 inimigos"))),
                                            Translate 516 334 (Color cor3 (Scale 0.16 0.16 (Text "Derrote 1000 inimigos"))),
                                            Translate (-137) (-333) (Color cor7 (Scale 0.16 0.16 (Text "Ganhe um nivel com pelo"))),
                                            Translate (-87) (-377) (Color cor7 (Scale 0.16 0.16 (Text "menos 20 torres"))),
                                            Translate 450 (-333) (Color cor8 (Scale 0.16 0.16 (Text "Ganhe um nivel com pelo menos"))),
                                            Translate 486 (-377) (Color cor8 (Scale 0.16 0.16 (Text "6 tipos diferentes de torres"))),
                                            Translate (-741) 16 (Color cor4 (Scale 0.16 0.16 (Text "Ganhe um nivel sem"))),
                                            Translate (-741) (-31) (Color cor4 (Scale 0.16 0.16 (Text "perder nenhuma vida"))),
                                            Translate 482 16 (Color cor5 (Scale 0.16 0.16 (Text "Ganhe o 'Vulcao das Trevas'"))),
                                            Translate 492 (-31) (Color cor5 (Scale 0.16 0.16 (Text "sem perder nenhuma vida"))),
                                            Translate (-713) (-333) (Color cor6 (Scale 0.16 0.16 (Text "Ganhe todos os"))),
                                            Translate (-707) (-377) (Color cor6 (Scale 0.16 0.16 (Text "niveis principais")))]
                                                where corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      imagemBackground
                                                            | tema == Natural = imagemBackgroundNatural
                                                            | tema == Flamejante = imagemBackgroundFlamejante
                                                            | otherwise = imagemBackgroundCristalina
                                                      cor1 = if q < 10 then greyN 0.5 else corTema
                                                      cor2 = if q < 100 then greyN 0.5 else corTema
                                                      cor3 = if q < 1000 then greyN 0.5 else corTema
                                                      cor4 = if 4 `elem` cs then corTema else greyN 0.5
                                                      cor5 = if 5 `elem` cs then corTema else greyN 0.5
                                                      cor6 = if 6 `elem` cs then corTema else greyN 0.5
                                                      cor7 = if 7 `elem` cs then corTema else greyN 0.5
                                                      cor8 = if 8 `elem` cs then corTema else greyN 0.5
    JogoPausado j _ _ _ tema b _ -> Pictures ([Translate (-924) 331 (
                                              desenhaMapa relva r3 r6 r9 r12 r15 r18 r21
                                                          terra t3 t6 t9 t12 t15 t18 t21
                                                          texturaAgua tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21
                                                          texturaLava tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105
                                                          texturaLagrimasCristalinas tLCE3 tLCE6 tLCE9 tLCE12 tLCE15 tLCE18 tLCE21 mapa td),
                                               pontes,desenhaDetalhes (numeroNivel j) (Scale 1.5 1.5 imagemArvore) (Scale 1.5 1.5 imagemArvoreMorta)
                                               (Scale 1.5 1.5 imagemCristalAzul) (Scale 1.5 1.5 imagemCristalMagenta),
                                               desenhaAlcanceTorre b posicao torres,
                                               Translate 677 454 (Scale 0.8 0.8 (desenhaBotaoGrande corTema)),
                                               Translate (-922) 306 (Color corBorda (desenhaBorda posicao)),
                                               Translate (-924) 331 (desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                                                                                   imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado
                                                                                   imagem1 imagem2 imagem3 torres),
                                               Translate (-924) 331 (desenhaBase base imagemBase),
                                               Translate 422 0 (Color corTema (rectangleSolid 12 1200)),
                                               Translate 0 397 (Color corTema (rectangleSolid 2000 12)),
                                               Translate 422 0 (Color black (rectangleSolid 8 1200)),
                                               Translate 0 397 (Color black (rectangleSolid 2000 8)),
                                               Translate 194 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-255) 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-703) 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-695) 430 (Scale 0.4 0.4 (Color black (Text (show (creditosBase base))))),
                                               Translate (-241) 430 (Scale 0.4 0.4 (Color black (Text (show (max 0 (round (vidaBase base))))))),
                                               Translate 213 430 (Scale 0.4 0.4 (Color black (Text (show (inimigosDerrotados j))))),
                                               Translate (-745) 454 (Scale 0.3 0.3 texturaOuro),
                                               Translate (-291) 454 (Scale 0.3 0.3 texturaVida),
                                               Translate 177 454 (Scale 0.3 0.3 inimigoDerrotado),
                                               Translate 552 278 (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 278 (Scale 2 2 imagemArqueiroFlamejante),
                                               Translate 526 194 (Scale 0.2 0.2 (Text (show (fst arqueiroFlamejante)))),
                                               Translate 488 208 (Scale 0.15 0.15 texturaOuro),
                                               Translate 488 342 (Scale 0.125 0.125 pFogo),
                                               Translate 616 342 (Scale 0.1 0.1 numero1),
                                               Translate 802 278 (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 278 (Scale 2 2 imagemSabioCorruptor),
                                               Translate 776 194 (Scale 0.2 0.2 (Text (show (fst sabioCorruptor)))),
                                               Translate 738 208 (Scale 0.15 0.15 texturaOuro),
                                               Translate 738 342 (Scale 0.125 0.125 pFraqueza),
                                               Translate 866 342 (Scale 0.1 0.1 numero2),
                                               Translate 552 52 (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 52 (Scale 2 2 imagemCarvalhoEnt),
                                               Translate 526 (-32) (Scale 0.2 0.2 (Text (show (fst carvalhoEnt)))),
                                               Translate 488 (-18) (Scale 0.15 0.15 texturaOuro),
                                               Translate 488 116 (Scale 0.125 0.125 pResina),
                                               Translate 616 116 (Scale 0.1 0.1 numero3),
                                               Translate 802 52 (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 52 (Scale 2 2 imagemMaquinaDeResina),
                                               Translate 776 (-32) (Scale 0.2 0.2 (Text (show (fst maquinaDeResina)))),
                                               Translate 738 (-18) (Scale 0.15 0.15 texturaOuro),
                                               Translate 738 116 (Scale 0.125 0.125 pResina),
                                               Translate 866 116 (Scale 0.1 0.1 numero4),
                                               Translate 552 (-174) (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 (-174) (Scale 2 2 corMagoDeGelo),
                                               Translate 526 (-254) (Scale 0.2 0.2 (Text (show (fst magoDeGelo)))),
                                               Translate 488 (-240) (Scale 0.15 0.15 corOuro2),
                                               Translate 488 (-110) (Scale 0.125 0.125 corGelo2),
                                               Translate 616 (-110) (Scale 0.1 0.1 numero5),
                                               Translate 802 (-174) (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 (-174) (Scale 2 2 corFornalhaArdente),
                                               Translate 776 (-254) (Scale 0.2 0.2 (Text (show (fst fornalhaArdente)))),
                                               Translate 738 (-240) (Scale 0.15 0.15 corOuro2),
                                               Translate 738 (-110) (Scale 0.125 0.125 corFogo2),
                                               Translate 866 (-110) (Scale 0.1 0.1 numero6),
                                               Translate 552 (-400) (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 (-400) (Scale 2 2 corCanhao),
                                               Translate 526 (-480) (Scale 0.2 0.2 (Text (show (fst canhao)))),
                                               Translate 488 (-466) (Scale 0.15 0.15 corOuro3),
                                               Translate 488 (-336) (Scale 0.125 0.125 corFraqueza3),
                                               Translate 616 (-336) (Scale 0.1 0.1 numero7),
                                               Translate 802 (-400) (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 (-400) (Scale 2 2 corJuizGelado),
                                               Translate 776 (-480) (Scale 0.2 0.2 (Text (show (fst juizGelado)))),
                                               Translate 738 (-466) (Scale 0.15 0.15 corOuro3),
                                               Translate 738 (-336) (Scale 0.125 0.125 corGelo3),
                                               Translate 866 (-336) (Scale 0.1 0.1 numero8)]
                                               ++ (map (Translate (-2) (-114) . desenhaPortal imagemPortal) portais
                                               ++ map (Translate (-2) (-114) . 
                                               desenhaInimigo imagemGharzul imagemZalrok imagemZalrokReforcado imagemFenrir imagemTharok imagemAranthisMortifera
                                               imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma imagemReaper imagemIgnaroth imagemDragaoCristalino
                                               imagemLeaoCristalino imagemFungalith imagemAethralith) inimigos ++ [Pictures (map (desenhaGota corChuva) pg)] ++ [Pictures vidaDeBoss]) 
                                               ++ [Translate (-256) (-188) (desenhaBotao corTema),
                                               Translate (-256) (-20) (desenhaBotao corTema),
                                               Translate (-256) 148 (desenhaBotao corTema),
                                               Translate 256 (-188) (desenhaBotao corTema),
                                               Translate 256 (-20) (desenhaBotao corTema),
                                               Translate 256 148 (desenhaBotao corTema),
                                               Translate 84 128 (Color corTema (Scale 0.4 0.4 (Text "Reiniciar Nivel"))),
                                               Translate (-356) 128 (Color corTema (Scale 0.4 0.4 (Text "Retomar"))),
                                               Translate (-402) (-208) (Color corTema (Scale 0.4 0.4 (Text "Mudar Nivel"))),
                                               Translate 76 (-208) (Color corTema (Scale 0.4 0.4 (Text "Menu Principal"))),
                                               Translate (-336) (-40) (Color corTema (Scale 0.4 0.4 (Text "Torres"))),
                                               Translate 158 (-40) (Color corTema (Scale 0.4 0.4 (Text "Inimigos")))])
                                                where Jogo base portais torres mapa inimigos loja _ posicao _ (_,_,pg) td = j
                                                      (x,y) = posicao
                                                      corBorda
                                                            | (x - 0.5,y - 0.5) `elem` filtraPosicoesRelva mapa (0,0) &&
                                                              (not (any (\t -> posicaoTorre t == (x,y)) torres) ||
                                                              (nivelTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) < 3
                                                              && creditosBase base >= determinaCustoMelhoraTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) loja)) = green
                                                            | otherwise = red
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      corMagoDeGelo
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemMagoDeGeloCinzenta
                                                            | otherwise = imagemMagoDeGelo
                                                      corFornalhaArdente
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemFornalhaArdenteCinzenta
                                                            | otherwise = imagemFornalhaArdente
                                                      corCanhao
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemCanhaoCinzenta
                                                            | otherwise = imagemCanhao
                                                      corJuizGelado
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemJuizGeladoCinzenta
                                                            | otherwise = imagemJuizGelado
                                                      corOuro2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corOuro3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corFogo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pFogoCinzenta
                                                            | otherwise = pFogo
                                                      corGelo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corGelo3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corFraqueza3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pFraquezaCinzenta
                                                            | otherwise = pFraqueza
                                                      (relva,r3,r6,r9,r12,r15,r18,r21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaCinzas,tCE3,tCE6,tCE9,tCE12,tCE15,tCE18,tCE21)
                                                            | otherwise = (texturaRelva,tRE3,tRE6,tRE9,tRE12,tRE15,tRE18,tRE21)
                                                      (terra,t3,t6,t9,t12,t15,t18,t21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaRocha,tPE3,tPE6,tPE9,tPE12,tPE15,tPE18,tPE21)
                                                            | otherwise = (texturaTerra,tTE3,tTE6,tTE9,tTE12,tTE15,tTE18,tTE21)
                                                      vidaDeBoss
                                                            | not (any (\i -> vidaMaxima i > 4999) inimigos) = []
                                                            | otherwise = [desenhaVidaDeBoss barraVidaDeBoss (head (filter (\i -> vidaMaxima i > 4999) inimigos))]
                                                      pontes
                                                            | numeroNivel j == 2 || numeroNivel j == 6 = Pictures [Translate (-192) (-86) ponteAgua,
                                                                                                                   Translate 192 (-86) ponteAgua]
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = Pictures [Translate (-640) 42 ponteLagrimasCristalinas]
                                                            | otherwise = Pictures []
                                                      corChuva
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = dark orange
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = dark magenta
                                                            | otherwise = dark blue
    GanhouJogo j _ _ _ tema b _ -> Pictures ([Translate (-924) 331 (
                                              desenhaMapa relva r3 r6 r9 r12 r15 r18 r21
                                                          terra t3 t6 t9 t12 t15 t18 t21
                                                          texturaAgua tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21
                                                          texturaLava tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105
                                                          texturaLagrimasCristalinas tLCE3 tLCE6 tLCE9 tLCE12 tLCE15 tLCE18 tLCE21 mapa td),
                                              pontes,desenhaDetalhes (numeroNivel j) (Scale 1.5 1.5 imagemArvore) (Scale 1.5 1.5 imagemArvoreMorta)
                                              (Scale 1.5 1.5 imagemCristalAzul) (Scale 1.5 1.5 imagemCristalMagenta),
                                              desenhaAlcanceTorre b posicao torres,
                                              Translate 677 454 (Scale 0.8 0.8 (desenhaBotaoGrande corTema)),
                                              Translate (-922) 306 (Color corBorda (desenhaBorda posicao)),
                                              Translate (-924) 331 (desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                                                                                  imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado
                                                                                  imagem1 imagem2 imagem3 torres),
                                              Translate (-924) 331 (desenhaBase base imagemBase),
                                              Translate 422 0 (Color corTema (rectangleSolid 12 1200)),
                                              Translate 0 397 (Color corTema (rectangleSolid 2000 12)),
                                              Translate 422 0 (Color black (rectangleSolid 8 1200)),
                                              Translate 0 397 (Color black (rectangleSolid 2000 8)),
                                              Translate 194 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-255) 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-703) 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-695) 430 (Scale 0.4 0.4 (Color black (Text (show (creditosBase base))))),
                                              Translate (-241) 430 (Scale 0.4 0.4 (Color black (Text (show (max 0 (round (vidaBase base))))))),
                                              Translate 213 430 (Scale 0.4 0.4 (Color black (Text (show (inimigosDerrotados j))))),
                                              Translate (-745) 454 (Scale 0.3 0.3 texturaOuro),
                                              Translate (-291) 454 (Scale 0.3 0.3 texturaVida),
                                              Translate 177 454 (Scale 0.3 0.3 inimigoDerrotado),
                                              Translate 552 278 (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 278 (Scale 2 2 imagemArqueiroFlamejante),
                                              Translate 526 194 (Scale 0.2 0.2 (Text (show (fst arqueiroFlamejante)))),
                                              Translate 488 208 (Scale 0.15 0.15 texturaOuro),
                                              Translate 488 342 (Scale 0.125 0.125 pFogo),
                                              Translate 616 342 (Scale 0.1 0.1 numero1),
                                              Translate 802 278 (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 278 (Scale 2 2 imagemSabioCorruptor),
                                              Translate 776 194 (Scale 0.2 0.2 (Text (show (fst sabioCorruptor)))),
                                              Translate 738 208 (Scale 0.15 0.15 texturaOuro),
                                              Translate 738 342 (Scale 0.125 0.125 pFraqueza),
                                              Translate 866 342 (Scale 0.1 0.1 numero2),
                                              Translate 552 52 (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 52 (Scale 2 2 imagemCarvalhoEnt),
                                              Translate 526 (-32) (Scale 0.2 0.2 (Text (show (fst carvalhoEnt)))),
                                              Translate 488 (-18) (Scale 0.15 0.15 texturaOuro),
                                              Translate 488 116 (Scale 0.125 0.125 pResina),
                                              Translate 616 116 (Scale 0.1 0.1 numero3),
                                              Translate 802 52 (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 52 (Scale 2 2 imagemMaquinaDeResina),
                                              Translate 776 (-32) (Scale 0.2 0.2 (Text (show (fst maquinaDeResina)))),
                                              Translate 738 (-18) (Scale 0.15 0.15 texturaOuro),
                                              Translate 738 116 (Scale 0.125 0.125 pResina),
                                              Translate 866 116 (Scale 0.1 0.1 numero4),
                                              Translate 552 (-174) (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 (-174) (Scale 2 2 corMagoDeGelo),
                                              Translate 526 (-254) (Scale 0.2 0.2 (Text (show (fst magoDeGelo)))),
                                              Translate 488 (-240) (Scale 0.15 0.15 corOuro2),
                                              Translate 488 (-110) (Scale 0.125 0.125 corGelo2),
                                              Translate 616 (-110) (Scale 0.1 0.1 numero5),
                                              Translate 802 (-174) (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 (-174) (Scale 2 2 corFornalhaArdente),
                                              Translate 776 (-254) (Scale 0.2 0.2 (Text (show (fst fornalhaArdente)))),
                                              Translate 738 (-240) (Scale 0.15 0.15 corOuro2),
                                              Translate 738 (-110) (Scale 0.125 0.125 corFogo2),
                                              Translate 866 (-110) (Scale 0.1 0.1 numero6),
                                              Translate 552 (-400) (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 (-400) (Scale 2 2 corCanhao),
                                              Translate 526 (-480) (Scale 0.2 0.2 (Text (show (fst canhao)))),
                                              Translate 488 (-466) (Scale 0.15 0.15 corOuro3),
                                              Translate 488 (-336) (Scale 0.125 0.125 corFraqueza3),
                                              Translate 616 (-336) (Scale 0.1 0.1 numero7),
                                              Translate 802 (-400) (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 (-400) (Scale 2 2 corJuizGelado),
                                              Translate 776 (-480) (Scale 0.2 0.2 (Text (show (fst juizGelado)))),
                                              Translate 738 (-466) (Scale 0.15 0.15 corOuro3),
                                              Translate 738 (-336) (Scale 0.125 0.125 corGelo3),
                                              Translate 866 (-336) (Scale 0.1 0.1 numero8)]
                                              ++ (map (Translate (-2) (-114) . desenhaPortal imagemPortal) portais
                                              ++ map (Translate (-2) (-114) . 
                                              desenhaInimigo imagemGharzul imagemZalrok imagemZalrokReforcado imagemFenrir imagemTharok imagemAranthisMortifera
                                              imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma imagemReaper imagemIgnaroth imagemDragaoCristalino
                                              imagemLeaoCristalino imagemFungalith imagemAethralith) inimigos ++ [Pictures (map (desenhaGota corChuva) pg)] ++ [Pictures vidaDeBoss])
                                              ++ [Translate 0 (-52) (desenhaBotao corTema),Translate (-177) (-74) (Color corTema (Scale 0.4 0.4 (Text "Reiniciar Nivel"))),
                                              Translate 0 116 (desenhaBotao corNivelSeguinte),Translate (-173) 94 (Color corNivelSeguinte (Scale 0.4 0.4 (Text "Nivel Seguinte"))),
                                              Translate 0 (-220) (desenhaBotao corTema),Translate (-149) (-242) (Color corTema (Scale 0.4 0.4 (Text "Mudar Nivel"))),
                                              Translate 0 (-388) (desenhaBotao corTema),Translate (-181 + x') (-410) (Color corTema (Scale 0.4 0.4 (Text (nivelNovo (numeroNivel j))))),
                                              Translate 0 310 (Scale 1 0.9 imagemVitoria)])
                                                where Jogo base portais torres mapa inimigos loja _ posicao _ (_,_,pg) td = j
                                                      (x,y) = posicao
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      corBorda
                                                            | (x - 0.5,y - 0.5) `elem` filtraPosicoesRelva mapa (0,0) &&
                                                              (not (any (\t -> posicaoTorre t == (x,y)) torres) ||
                                                              (nivelTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) < 3
                                                              && creditosBase base >= determinaCustoMelhoraTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) loja)) = green
                                                            | otherwise = red
                                                      nivelNovo n
                                                            | n < 5 = "Nivel em Elite"
                                                            | n > 4 = "Nivel Base"
                                                      corMagoDeGelo
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemMagoDeGeloCinzenta
                                                            | otherwise = imagemMagoDeGelo
                                                      corFornalhaArdente
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemFornalhaArdenteCinzenta
                                                            | otherwise = imagemFornalhaArdente
                                                      corCanhao
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemCanhaoCinzenta
                                                            | otherwise = imagemCanhao
                                                      corJuizGelado
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemJuizGeladoCinzenta
                                                            | otherwise = imagemJuizGelado
                                                      corOuro2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corOuro3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corFogo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pFogoCinzenta
                                                            | otherwise = pFogo
                                                      corGelo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corGelo3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corFraqueza3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pFraquezaCinzenta
                                                            | otherwise = pFraqueza
                                                      x' = if numeroNivel j < 5 then 0 else 48
                                                      (relva,r3,r6,r9,r12,r15,r18,r21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaCinzas,tCE3,tCE6,tCE9,tCE12,tCE15,tCE18,tCE21)
                                                            | otherwise = (texturaRelva,tRE3,tRE6,tRE9,tRE12,tRE15,tRE18,tRE21)
                                                      (terra,t3,t6,t9,t12,t15,t18,t21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaRocha,tPE3,tPE6,tPE9,tPE12,tPE15,tPE18,tPE21)
                                                            | otherwise = (texturaTerra,tTE3,tTE6,tTE9,tTE12,tTE15,tTE18,tTE21)
                                                      corNivelSeguinte
                                                            | numeroNivel j == 3 || numeroNivel j == 4 || numeroNivel j == 7 || numeroNivel j == 8 = greyN 0.5
                                                            | otherwise = corTema
                                                      vidaDeBoss
                                                            | not (any (\i -> vidaMaxima i > 4999) inimigos) = []
                                                            | otherwise = [desenhaVidaDeBoss barraVidaDeBoss (head (filter (\i -> vidaMaxima i > 4999) inimigos))]
                                                      pontes
                                                            | numeroNivel j == 2 || numeroNivel j == 6 = Pictures [Translate (-192) (-86) ponteAgua,
                                                                                                                   Translate 192 (-86) ponteAgua]
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = Pictures [Translate (-640) 42 ponteLagrimasCristalinas]
                                                            | otherwise = Pictures []
                                                      corChuva
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = dark orange
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = dark magenta
                                                            | otherwise = dark blue
    PerdeuJogo j _ _ _ tema b _ -> Pictures ([Translate (-924) 331 (
                                              desenhaMapa relva r3 r6 r9 r12 r15 r18 r21
                                                          terra t3 t6 t9 t12 t15 t18 t21
                                                          texturaAgua tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21
                                                          texturaLava tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105
                                                          texturaLagrimasCristalinas tLCE3 tLCE6 tLCE9 tLCE12 tLCE15 tLCE18 tLCE21 mapa td),
                                              pontes,desenhaDetalhes (numeroNivel j) (Scale 1.5 1.5 imagemArvore) (Scale 1.5 1.5 imagemArvoreMorta)
                                              (Scale 1.5 1.5 imagemCristalAzul) (Scale 1.5 1.5 imagemCristalMagenta),
                                              desenhaAlcanceTorre b posicao torres,
                                              Translate 677 454 (Scale 0.8 0.8 (desenhaBotaoGrande corTema)),
                                              Translate (-922) 306 (Color corBorda (desenhaBorda posicao)),
                                              Translate (-924) 331 (desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                                                                                  imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado
                                                                                  imagem1 imagem2 imagem3 torres),
                                              Translate (-924) 331 (desenhaBase base imagemBase),
                                              Translate 422 0 (Color corTema (rectangleSolid 12 1200)),
                                              Translate 0 397 (Color corTema (rectangleSolid 2000 12)),
                                              Translate 422 0 (Color black (rectangleSolid 8 1200)),
                                              Translate 0 397 (Color black (rectangleSolid 2000 8)),
                                              Translate 194 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-255) 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-703) 455 (Scale 0.54 0.42 tabelaMadeira),
                                              Translate (-695) 430 (Scale 0.4 0.4 (Color black (Text (show (creditosBase base))))),
                                              Translate (-241) 430 (Scale 0.4 0.4 (Color black (Text (show (max 0 (round (vidaBase base))))))),
                                              Translate 213 430 (Scale 0.4 0.4 (Color black (Text (show (inimigosDerrotados j))))),
                                              Translate (-745) 454 (Scale 0.3 0.3 texturaOuro),
                                              Translate (-291) 454 (Scale 0.3 0.3 texturaVida),
                                              Translate 177 454 (Scale 0.3 0.3 inimigoDerrotado),
                                              Translate 552 278 (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 278 (Scale 2 2 imagemArqueiroFlamejante),
                                              Translate 526 194 (Scale 0.2 0.2 (Text (show (fst arqueiroFlamejante)))),
                                              Translate 488 208 (Scale 0.15 0.15 texturaOuro),
                                              Translate 488 342 (Scale 0.125 0.125 pFogo),
                                              Translate 616 342 (Scale 0.1 0.1 numero1),
                                              Translate 802 278 (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 278 (Scale 2 2 imagemSabioCorruptor),
                                              Translate 776 194 (Scale 0.2 0.2 (Text (show (fst sabioCorruptor)))),
                                              Translate 738 208 (Scale 0.15 0.15 texturaOuro),
                                              Translate 738 342 (Scale 0.125 0.125 pFraqueza),
                                              Translate 866 342 (Scale 0.1 0.1 numero2),
                                              Translate 552 52 (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 52 (Scale 2 2 imagemCarvalhoEnt),
                                              Translate 526 (-32) (Scale 0.2 0.2 (Text (show (fst carvalhoEnt)))),
                                              Translate 488 (-18) (Scale 0.15 0.15 texturaOuro),
                                              Translate 488 116 (Scale 0.125 0.125 pResina),
                                              Translate 616 116 (Scale 0.1 0.1 numero3),
                                              Translate 802 52 (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 52 (Scale 2 2 imagemMaquinaDeResina),
                                              Translate 776 (-32) (Scale 0.2 0.2 (Text (show (fst maquinaDeResina)))),
                                              Translate 738 (-18) (Scale 0.15 0.15 texturaOuro),
                                              Translate 738 116 (Scale 0.125 0.125 pResina),
                                              Translate 866 116 (Scale 0.1 0.1 numero4),
                                              Translate 552 (-174) (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 (-174) (Scale 2 2 corMagoDeGelo),
                                              Translate 526 (-254) (Scale 0.2 0.2 (Text (show (fst magoDeGelo)))),
                                              Translate 488 (-240) (Scale 0.15 0.15 corOuro2),
                                              Translate 488 (-110) (Scale 0.125 0.125 corGelo2),
                                              Translate 616 (-110) (Scale 0.1 0.1 numero5),
                                              Translate 802 (-174) (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 (-174) (Scale 2 2 corFornalhaArdente),
                                              Translate 776 (-254) (Scale 0.2 0.2 (Text (show (fst fornalhaArdente)))),
                                              Translate 738 (-240) (Scale 0.15 0.15 corOuro2),
                                              Translate 738 (-110) (Scale 0.125 0.125 corFogo2),
                                              Translate 866 (-110) (Scale 0.1 0.1 numero6),
                                              Translate 552 (-400) (Scale 0.48 0.48 placaMadeira),
                                              Translate 552 (-400) (Scale 2 2 corCanhao),
                                              Translate 526 (-480) (Scale 0.2 0.2 (Text (show (fst canhao)))),
                                              Translate 488 (-466) (Scale 0.15 0.15 corOuro3),
                                              Translate 488 (-336) (Scale 0.125 0.125 corFraqueza3),
                                              Translate 616 (-336) (Scale 0.1 0.1 numero7),
                                              Translate 802 (-400) (Scale 0.48 0.48 placaMadeira),
                                              Translate 802 (-400) (Scale 2 2 corJuizGelado),
                                              Translate 776 (-480) (Scale 0.2 0.2 (Text (show (fst juizGelado)))),
                                              Translate 738 (-466) (Scale 0.15 0.15 corOuro3),
                                              Translate 738 (-336) (Scale 0.125 0.125 corGelo3),
                                              Translate 866 (-336) (Scale 0.1 0.1 numero8)]
                                              ++ (map (Translate (-2) (-114) . desenhaPortal imagemPortal) portais
                                              ++ map (Translate (-2) (-114) . 
                                              desenhaInimigo imagemGharzul imagemZalrok imagemZalrokReforcado imagemFenrir imagemTharok imagemAranthisMortifera
                                              imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma imagemReaper imagemIgnaroth imagemDragaoCristalino
                                              imagemLeaoCristalino imagemFungalith imagemAethralith) inimigos ++ [Pictures vidaDeBoss]) ++ [Pictures (map (desenhaGota corChuva) pg)]
                                              ++ [Translate 0 (-95) (desenhaBotao corTema),Translate (-149) (-117) (Color corTema (Scale 0.4 0.4 (Text "Mudar Nivel"))),
                                              Translate 0 96 (desenhaBotao corTema),Translate (-177) 74 (Color corTema (Scale 0.4 0.4 (Text "Reiniciar Nivel"))),
                                              Translate 0 310 (Scale 1 0.9 imagemDerrota)])
                                                where Jogo base portais torres mapa inimigos loja _ posicao _ (_,_,pg) td = j
                                                      (x,y) = posicao
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      corBorda
                                                            | (x - 0.5,y - 0.5) `elem` filtraPosicoesRelva mapa (0,0) &&
                                                              (not (any (\t -> posicaoTorre t == (x,y)) torres) ||
                                                              (nivelTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) < 3
                                                              && creditosBase base >= determinaCustoMelhoraTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) loja)) = green
                                                            | otherwise = red
                                                      corMagoDeGelo
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemMagoDeGeloCinzenta
                                                            | otherwise = imagemMagoDeGelo
                                                      corFornalhaArdente
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemFornalhaArdenteCinzenta
                                                            | otherwise = imagemFornalhaArdente
                                                      corCanhao
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemCanhaoCinzenta
                                                            | otherwise = imagemCanhao
                                                      corJuizGelado
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemJuizGeladoCinzenta
                                                            | otherwise = imagemJuizGelado
                                                      corOuro2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corOuro3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corFogo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pFogoCinzenta
                                                            | otherwise = pFogo
                                                      corGelo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corGelo3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corFraqueza3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pFraquezaCinzenta
                                                            | otherwise = pFraqueza
                                                      (relva,r3,r6,r9,r12,r15,r18,r21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaCinzas,tCE3,tCE6,tCE9,tCE12,tCE15,tCE18,tCE21)
                                                            | otherwise = (texturaRelva,tRE3,tRE6,tRE9,tRE12,tRE15,tRE18,tRE21)
                                                      (terra,t3,t6,t9,t12,t15,t18,t21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaRocha,tPE3,tPE6,tPE9,tPE12,tPE15,tPE18,tPE21)
                                                            | otherwise = (texturaTerra,tTE3,tTE6,tTE9,tTE12,tTE15,tTE18,tTE21)
                                                      vidaDeBoss
                                                            | not (any (\i -> vidaMaxima i > 4999) inimigos) = []
                                                            | otherwise = [desenhaVidaDeBoss barraVidaDeBoss (head (filter (\i -> vidaMaxima i > 4999) inimigos))]
                                                      pontes
                                                            | numeroNivel j == 2 || numeroNivel j == 6 = Pictures [Translate (-192) (-86) ponteAgua,
                                                                                                                   Translate 192 (-86) ponteAgua]
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = Pictures [Translate (-640) 42 ponteLagrimasCristalinas]
                                                            | otherwise = Pictures []
                                                      corChuva
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = dark orange
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = dark magenta
                                                            | otherwise = dark blue
    JogoACorrer j _ _ _ tema b _ -> Pictures ([Translate (-924) 331 (
                                               desenhaMapa relva r3 r6 r9 r12 r15 r18 r21
                                                           terra t3 t6 t9 t12 t15 t18 t21
                                                           texturaAgua tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21
                                                           texturaLava tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105
                                                           texturaLagrimasCristalinas tLCE3 tLCE6 tLCE9 tLCE12 tLCE15 tLCE18 tLCE21 mapa td),
                                               pontes, desenhaDetalhes (numeroNivel j) (Scale 1.5 1.5 imagemArvore) (Scale 1.5 1.5 imagemArvoreMorta)
                                               (Scale 1.5 1.5 imagemCristalAzul) (Scale 1.5 1.5 imagemCristalMagenta),
                                               desenhaAlcanceTorre b posicao torres,
                                               Translate 677 454 (Scale 0.8 0.8 (desenhaBotaoGrande corTema)),
                                               Translate 610 434 (Color corTema (Scale 0.4 0.4 (Text "Pausa"))),
                                               Translate (-922) 306 (Color corBorda (desenhaBorda posicao)),
                                               Translate (-924) 331 (desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                                                                                   imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado
                                                                                   imagem1 imagem2 imagem3 torres),
                                               Translate (-924) 331 (desenhaBase base imagemBase),
                                               Translate 422 0 (Color corTema (rectangleSolid 12 1200)),
                                               Translate 0 397 (Color corTema (rectangleSolid 2000 12)),
                                               Translate 422 0 (Color black (rectangleSolid 8 1200)),
                                               Translate 0 397 (Color black (rectangleSolid 2000 8)),
                                               Translate 194 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-255) 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-703) 455 (Scale 0.54 0.42 tabelaMadeira),
                                               Translate (-695) 430 (Scale 0.4 0.4 (Color black (Text (show (creditosBase base))))),
                                               Translate (-241) 430 (Scale 0.4 0.4 (Color black (Text (show (max 0 (round (vidaBase base))))))),
                                               Translate 213 430 (Scale 0.4 0.4 (Color black (Text (show (inimigosDerrotados j))))),
                                               Translate (-745) 454 (Scale 0.3 0.3 texturaOuro),
                                               Translate (-291) 454 (Scale 0.3 0.3 texturaVida),
                                               Translate 177 454 (Scale 0.3 0.3 inimigoDerrotado),
                                               Translate 552 278 (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 278 (Scale 2 2 imagemArqueiroFlamejante),
                                               Translate 526 194 (Scale 0.2 0.2 (Text (show (fst arqueiroFlamejante)))),
                                               Translate 488 208 (Scale 0.15 0.15 texturaOuro),
                                               Translate 488 342 (Scale 0.125 0.125 pFogo),
                                               Translate 616 342 (Scale 0.1 0.1 numero1),
                                               Translate 802 278 (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 278 (Scale 2 2 imagemSabioCorruptor),
                                               Translate 776 194 (Scale 0.2 0.2 (Text (show (fst sabioCorruptor)))),
                                               Translate 738 208 (Scale 0.15 0.15 texturaOuro),
                                               Translate 738 342 (Scale 0.125 0.125 pFraqueza),
                                               Translate 866 342 (Scale 0.1 0.1 numero2),
                                               Translate 552 52 (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 52 (Scale 2 2 imagemCarvalhoEnt),
                                               Translate 526 (-32) (Scale 0.2 0.2 (Text (show (fst carvalhoEnt)))),
                                               Translate 488 (-18) (Scale 0.15 0.15 texturaOuro),
                                               Translate 488 116 (Scale 0.125 0.125 pResina),
                                               Translate 616 116 (Scale 0.1 0.1 numero3),
                                               Translate 802 52 (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 52 (Scale 2 2 imagemMaquinaDeResina),
                                               Translate 776 (-32) (Scale 0.2 0.2 (Text (show (fst maquinaDeResina)))),
                                               Translate 738 (-18) (Scale 0.15 0.15 texturaOuro),
                                               Translate 738 116 (Scale 0.125 0.125 pResina),
                                               Translate 866 116 (Scale 0.1 0.1 numero4),
                                               Translate 552 (-174) (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 (-174) (Scale 2 2 corMagoDeGelo),
                                               Translate 526 (-254) (Scale 0.2 0.2 (Text (show (fst magoDeGelo)))),
                                               Translate 488 (-240) (Scale 0.15 0.15 corOuro2),
                                               Translate 488 (-110) (Scale 0.125 0.125 corGelo2),
                                               Translate 616 (-110) (Scale 0.1 0.1 numero5),
                                               Translate 802 (-174) (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 (-174) (Scale 2 2 corFornalhaArdente),
                                               Translate 776 (-254) (Scale 0.2 0.2 (Text (show (fst fornalhaArdente)))),
                                               Translate 738 (-240) (Scale 0.15 0.15 corOuro2),
                                               Translate 738 (-110) (Scale 0.125 0.125 corFogo2),
                                               Translate 866 (-110) (Scale 0.1 0.1 numero6),
                                               Translate 552 (-400) (Scale 0.48 0.48 placaMadeira),
                                               Translate 552 (-400) (Scale 2 2 corCanhao),
                                               Translate 526 (-480) (Scale 0.2 0.2 (Text (show (fst canhao)))),
                                               Translate 488 (-466) (Scale 0.15 0.15 corOuro3),
                                               Translate 488 (-336) (Scale 0.125 0.125 corFraqueza3),
                                               Translate 616 (-336) (Scale 0.1 0.1 numero7),
                                               Translate 802 (-400) (Scale 0.48 0.48 placaMadeira),
                                               Translate 802 (-400) (Scale 2 2 corJuizGelado),
                                               Translate 776 (-480) (Scale 0.2 0.2 (Text (show (fst juizGelado)))),
                                               Translate 738 (-466) (Scale 0.15 0.15 corOuro3),
                                               Translate 738 (-336) (Scale 0.125 0.125 corGelo3),
                                               Translate 866 (-336) (Scale 0.1 0.1 numero8)]
                                               ++ (map (Translate (-2) (-114) . desenhaPortal imagemPortal) portais ++ map (Translate (-2) (-114) . 
                                               desenhaInimigo imagemGharzul imagemZalrok imagemZalrokReforcado imagemFenrir imagemTharok imagemAranthisMortifera
                                               imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma imagemReaper imagemIgnaroth imagemDragaoCristalino
                                               imagemLeaoCristalino imagemFungalith imagemAethralith) inimigos) ++ [Pictures vidaDeBoss]
                                               ++ [Pictures (map (desenhaGota corChuva) pg)])
                                                where Jogo base portais torres mapa inimigos loja _ posicao _ (_,_,pg) td = j
                                                      (x,y) = posicao
                                                      corBorda
                                                            | (x - 0.5,y - 0.5) `elem` filtraPosicoesRelva mapa (0,0) &&
                                                              (not (any (\t -> posicaoTorre t == (x,y)) torres) ||
                                                              (nivelTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) < 3
                                                              && creditosBase base >= determinaCustoMelhoraTorre (head (filter (\t -> posicaoTorre t == (x,y)) torres)) loja)) = green
                                                            | otherwise = red
                                                      corTema
                                                            | tema == Natural = verdeMedio
                                                            | tema == Flamejante = red
                                                            | otherwise = dark magenta
                                                      corMagoDeGelo
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemMagoDeGeloCinzenta
                                                            | otherwise = imagemMagoDeGelo
                                                      corFornalhaArdente
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = imagemFornalhaArdenteCinzenta
                                                            | otherwise = imagemFornalhaArdente
                                                      corCanhao
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemCanhaoCinzenta
                                                            | otherwise = imagemCanhao
                                                      corJuizGelado
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = imagemJuizGeladoCinzenta
                                                            | otherwise = imagemJuizGelado
                                                      corOuro2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corOuro3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = texturaOuroCinzenta
                                                            | otherwise = texturaOuro
                                                      corFogo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pFogoCinzenta
                                                            | otherwise = pFogo
                                                      corGelo2
                                                            | numeroNivel j == 1 || numeroNivel j == 5 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corGelo3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pGeloCinzenta
                                                            | otherwise = pGelo
                                                      corFraqueza3
                                                            | numeroNivel j < 3 || numeroNivel j == 5 || numeroNivel j == 6 = pFraquezaCinzenta
                                                            | otherwise = pFraqueza
                                                      (relva,r3,r6,r9,r12,r15,r18,r21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaCinzas,tCE3,tCE6,tCE9,tCE12,tCE15,tCE18,tCE21)
                                                            | otherwise = (texturaRelva,tRE3,tRE6,tRE9,tRE12,tRE15,tRE18,tRE21)
                                                      (terra,t3,t6,t9,t12,t15,t18,t21)
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = (texturaRocha,tPE3,tPE6,tPE9,tPE12,tPE15,tPE18,tPE21)
                                                            | otherwise = (texturaTerra,tTE3,tTE6,tTE9,tTE12,tTE15,tTE18,tTE21)
                                                      vidaDeBoss
                                                            | not (any (\i -> vidaMaxima i > 4999) inimigos) = []
                                                            | otherwise = [desenhaVidaDeBoss barraVidaDeBoss (head (filter (\i -> vidaMaxima i > 4999) inimigos))]
                                                      pontes
                                                            | numeroNivel j == 2 || numeroNivel j == 6 = Pictures [Translate (-192) (-86) ponteAgua,
                                                                                                                   Translate 192 (-86) ponteAgua]
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = Pictures [Translate (-640) 42 ponteLagrimasCristalinas]
                                                            | otherwise = Pictures []
                                                      corChuva
                                                            | numeroNivel j == 3 || numeroNivel j == 7 = dark orange
                                                            | numeroNivel j == 4 || numeroNivel j == 8 = dark magenta
                                                            | otherwise = dark blue
                                                      




-- {- Funções Auxiliares -} --

-- | Desenha uma gota de chuva (de água, lava ou lágrimas cristalinas).

desenhaGota :: Color -> Posicao -> Picture
desenhaGota cor (x,y) = Translate (-929 + x) (386 + y) (Color cor (Pictures [rectangleSolid 2 12,Translate (-2) (-12) (rectangleSolid 2 12),
                                                                   Translate (-4) (-24) (rectangleSolid 2 12),Translate (-6) (-36) (rectangleSolid 2 12)]))

-- | A função 'desenhaDetalhes' desenha os detalhes, tais como árvores e cristais, do mapa do jogo.

desenhaDetalhes :: Int -> Picture -> Picture -> Picture -> Picture -> Picture
desenhaDetalhes nNivel arvore arvoreMorta cristalAzul cristalMagenta
      | nNivel == 1 || nNivel == 5 = desenhaDetalhesPlaniciesVerdejantes
      | nNivel == 2 || nNivel == 6 = desenhaDetalhesFlorestaDaMorte
      | nNivel == 3 || nNivel == 7 = desenhaDetalhesVulcaoDasTrevas
      | otherwise = desenhaDetalhesIlhasCristalinas
            where desenhaDetalhesPlaniciesVerdejantes = Pictures [Translate (-400) 300 arvore,Translate (-650) 340 arvore,Translate (-830) 160 arvore,
                                                                  Translate (-360) 270 arvore,Translate (-50) (-180) arvore,Translate (-70) (-200) arvore,
                                                                  Translate (-330) (-80) arvore,Translate 150 50 arvore,Translate 200 (-30) arvore,
                                                                  Translate (-580) (-300) arvore,Translate (-620) (-310) arvore,Translate (-630) (-285) arvore,
                                                                  Translate (-60) (-420) arvore,Translate (-90) (-480) arvore,Translate 90 (-470) arvore,
                                                                  Translate (-850) (-465) arvore,Translate (-550) 30 arvore]
                  desenhaDetalhesFlorestaDaMorte = Pictures [Translate (-600) 0 arvore,Translate (-630) 20 arvore,Translate (-640) 50 arvore,Translate (-670) 30 arvore,
                                                             Translate (-680) (-10) arvore,Translate (-685) 35 arvore,Translate (-690) 50 arvore,Translate (-710) 20 arvore,
                                                             Translate (-580) 290 arvoreMorta,Translate (-550) 270 arvoreMorta,Translate (-530) 300 arvoreMorta,
                                                             Translate (-510) 280 arvoreMorta,Translate (-480) 300 arvoreMorta,Translate (-460) 310 arvoreMorta,
                                                             Translate (-465) 340 arvoreMorta,Translate (-240) 250 arvore,Translate (-210) 240 arvore,
                                                             Translate (-220) 220 arvore,Translate (-225) 270 arvore,Translate (-230) 280 arvore,Translate 300 200 arvore,
                                                             Translate (-190) 285 arvore,Translate (-175) 290 arvore,Translate (-170) 265 arvore,Translate 260 220 arvore,
                                                             Translate 275 240 arvore,Translate 310 250 arvore,Translate 305 (-350) arvore,Translate 320 (-370) arvore,
                                                             Translate 280 (-360) arvore]
                  desenhaDetalhesVulcaoDasTrevas = Pictures [Translate (-760) (-300) arvoreMorta,Translate (-790) (-320) arvoreMorta,Translate (-810) (-340) arvoreMorta,
                                                             Translate (-830) (-290) arvoreMorta,Translate (-870) (-305) arvoreMorta,Translate (-825) (-420) arvoreMorta,
                                                             Translate (-850) (-440) arvoreMorta,Translate (-600) 300 arvoreMorta,Translate (-560) 330 arvoreMorta,
                                                             Translate (-570) 360 arvoreMorta,Translate (-250) 260 arvoreMorta,Translate (-210) 230 arvoreMorta,
                                                             Translate (-220) 200 arvoreMorta,Translate 120 330 arvoreMorta,Translate 110 370 arvoreMorta,
                                                             Translate (-230) 0 arvoreMorta,Translate (-250) 40 arvoreMorta,Translate (-235) 60 arvoreMorta,
                                                             Translate 125 10 arvoreMorta,Translate 100 30 arvoreMorta,Translate 110 50 arvoreMorta,
                                                             Translate 70 40 arvoreMorta]
                  desenhaDetalhesIlhasCristalinas = Pictures [Translate (-800) (-20) cristalAzul,Translate (-880) (-40) cristalAzul,Translate (-870) 120 cristalAzul,
                                                              Translate (-840) 300 cristalMagenta,Translate (-860) 340 cristalMagenta,Translate 280 230 cristalAzul,
                                                              Translate 240 300 cristalMagenta,Translate 265 340 cristalMagenta,Translate 360 (-100) cristalAzul,
                                                              Translate 340 (-150) cristalAzul,Translate 370 (-230) cristalAzul,Translate 330 (-240) cristalAzul,
                                                              Translate 0 (-460) cristalMagenta,Translate (-170) (-410) cristalAzul,Translate (-210) (-430) cristalMagenta,
                                                              Translate (-750) (-300) cristalMagenta,Translate (-800) (-340) cristalMagenta,Translate (-310) 280 cristalAzul,
                                                              Translate (-340) 230 cristalMagenta,Translate 50 (-200) cristalMagenta,Translate 30 (-150) cristalMagenta]

-- | A função 'desenhaVidaDeBoss' desenha a barra de vida de um boss caso este exista.

desenhaVidaDeBoss :: Picture -> Inimigo -> Picture
desenhaVidaDeBoss barraVidaDeBoss boss = Pictures [Translate (-256) 356 barraVidaDeBoss,Translate (-255) 346 (Color red (rectangleSolid (794 * percentagemVida) 36)),
                                                   nomeBoss]
      where vida = vidaInimigo boss
            vMax = vidaMaxima boss
            percentagemVida = vida / vMax
            nomeBoss
                  | vMax > 19999 = Translate (-302) 338 (Color white (Scale 0.2 0.2 (Text "Ignaroth")))
                  | otherwise = Translate (-308) 338 (Color white (Scale 0.2 0.2 (Text "Aethralith")))

-- | A função 'filtraPosicoesAgua' determina as coordenadas de pontos correnpondentes apenas ao terreno de 'Agua' de um mapa.

filtraPosicoesAgua ::
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição do ponto na matriz (deve ser (0,0) inicialmente para determinar corretamente as coordenadas).
 Posicao ->
 -- | Posições correpondentes ao terreno de 'Agua'.
 [Posicao]
filtraPosicoesAgua [] _ = []
filtraPosicoesAgua (l:ls) (x,y) = filtraPosicoesAguaAux l (0,y) ++ filtraPosicoesAgua ls (x,y + 1)
    where filtraPosicoesAguaAux [] _ = []
          filtraPosicoesAguaAux (c:cs) (a,b)
            | c == Agua = (a,b) : filtraPosicoesAguaAux cs (a + 1,b)
            | otherwise = filtraPosicoesAguaAux cs (a + 1,b)

-- | A função 'filtraPosicoesLava' determina as coordenadas de pontos correnpondentes apenas ao terreno de 'Lava' de um mapa.

filtraPosicoesLava ::
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição do ponto na matriz (deve ser (0,0) inicialmente para determinar corretamente as coordenadas).
 Posicao ->
 -- | Posições correpondentes ao terreno de 'Lava'.
 [Posicao]
filtraPosicoesLava [] _ = []
filtraPosicoesLava (l:ls) (x,y) = filtraPosicoesLavaAux l (0,y) ++ filtraPosicoesLava ls (x,y + 1)
    where filtraPosicoesLavaAux [] _ = []
          filtraPosicoesLavaAux (c:cs) (a,b)
            | c == Lava = (a,b) : filtraPosicoesLavaAux cs (a + 1,b)
            | otherwise = filtraPosicoesLavaAux cs (a + 1,b)

-- | Uma cor diferente de verde.

verdeMedio :: Color
verdeMedio = makeColor (60 / 255) (180 /255) (60 / 255) 1

-- | Desenha um botão simples com borda da cor dada.

desenhaBotao :: Color -> Picture
desenhaBotao cor = Pictures [Color cor (rectangleSolid 384 128), Color (greyN 0.05) (rectangleSolid 368 112),Translate (-132) (-60) (Pictures
                             [Translate 288 8 e,Translate 256 8 c,Translate (-48) 8 e,Translate 0 8 e,Translate 216 8 c,Translate 64 8 c,Translate 152 8 c,Translate 8 8 e,Translate 120 8 c, -- Linha 1
                              Translate 280 16 e,Translate 0 16 e, -- Linha 2
                              Translate (-40) 24 e,Translate (-16) 24 e,Translate 104 24 c,Translate 208 24 c,Translate 152 24 c, -- Linha 3
                              Translate 296 32 c,Translate 256 32 e,Translate 112 32 e,Translate 96 32 e,Translate 208 32 e,Translate 88 32 e,Translate 48 32 c,Translate 128 32 e, -- Linha 4
                              Translate 288 40 c,Translate (-40) 40 c,Translate 136 40 e,Translate 240 40 e,Translate 200 40 e,Translate 72 40 e, -- Linha 5
                              Translate (-48) 48 e,Translate (-32) 48 e,Translate (-24) 48 c,Translate 112 48 e,Translate 16 48 e,Translate 8 48 c,Translate 192 48 e,Translate 136 48 e,Translate 176 48 e, -- Linha 6
                              Translate 288 56 c,Translate 256 56 e,Translate 248 56 c,Translate 128 56 c,Translate 192 56 c,Translate 240 56 e, -- Linha 7
                              Translate 280 64 c,Translate 264 64 e,Translate (-16) 64 c,Translate (-8) 64 e,Translate 80 64 e,Translate 240 64 c, -- Linha 8
                              Translate 296 72 c,Translate 288 72 e,Translate (-40) 72 c,Translate 216 72 c,Translate 64 72 c,Translate 152 72 c,Translate 8 72 e,Translate 120 72 c, -- Linha 9
                              Translate 280 80 c,Translate 256 80 c,Translate (-48) 80 e,Translate (-40) 80 e, -- Linha 10
                              Translate 264 88 c,Translate (-48) 88 c,Translate (-40) 88 e,Translate 104 88 c,Translate 208 88 c,Translate 152 88 c, -- Linha 11
                              Translate 312 96 e,Translate 264 96 e,Translate (-24) 96 c,Translate (-16) 96 e,Translate 0 96 c,Translate 112 96 e,Translate 96 96 e,Translate 208 96 e,Translate 88 96 e,Translate 48 96 c,Translate 128 96 e, -- Linha 12
                              Translate 296 104 c,Translate 0 104 c,Translate 136 104 e,Translate 240 104 e,Translate 200 104 e,Translate 72 104 e, -- Linha 13
                              Translate 312 112 c,Translate 280 112 e,Translate (-32) 112 c,Translate (-24) 112 c,Translate 112 112 e,Translate 16 112 e,Translate 8 112 c,Translate 192 112 e,Translate 136 112 e,Translate 176 112 e])] -- Linha 14
                                    where c = Color (greyN 0.15) (rectangleSolid 8 8)
                                          e = Color (greyN 0.2) (rectangleSolid 8 8)

-- | Desenha um botão simples (maior que a 'desenhaBotao') com borda da cor dada.

desenhaBotaoGrande :: Color -> Picture
desenhaBotaoGrande cor = Pictures [Color cor (rectangleSolid 622 128), Color (greyN 0.05) (rectangleSolid 606 112),Translate (-132) (-60) (Pictures
                                   [Translate (-142) 8 e,Translate 424 8 e,Translate 384 8 e,Translate 344 8 e,Translate 288 8 e,Translate 256 8 c,Translate (-48) 8 e,Translate 0 8 e,Translate 216 8 c,Translate 64 8 c,Translate 152 8 c,Translate 8 8 e,Translate 120 8 c, -- Linha 1
                                    Translate (-112) 16 e,Translate (-104) 16 e,Translate 400 16 e,Translate 360 16 e,Translate 280 16 e,Translate 0 16 e, -- Linha 2
                                    Translate (-120) 24 c,Translate (-88) 24 c,Translate 320 24 e,Translate (-40) 24 e,Translate (-16) 24 e,Translate 104 24 c,Translate 208 24 c,Translate 152 24 c, -- Linha 3
                                    Translate (-160) 32 c,Translate (-120) 32 c,Translate 296 32 c,Translate 256 32 e,Translate 112 32 e,Translate 96 32 e,Translate 208 32 e,Translate 88 32 e,Translate 48 32 c,Translate 128 32 e, -- Linha 4
                                    Translate (-128) 40 e,Translate (-80) 40 c,Translate 328 40 e,Translate 320 40 e,Translate 288 40 c,Translate (-40) 40 c,Translate 136 40 e,Translate 240 40 e,Translate 200 40 e,Translate 72 40 e, -- Linha 5
                                    Translate (-144) 48 e,Translate (-112) 48 c,Translate (-80) 48 c,Translate 400 48 e,Translate 376 48 e,Translate 328 48 c,Translate (-48) 48 e,Translate (-32) 48 e,Translate (-24) 48 c,Translate 112 48 e,Translate 16 48 e,Translate 8 48 c,Translate 192 48 e,Translate 136 48 e,Translate 176 48 e, -- Linha 6
                                    Translate (-152) 56 c,Translate (-104) 56 c,Translate (-88) 56 e,Translate 328 56 c,Translate 288 56 c,Translate 256 56 e,Translate 248 56 c,Translate 128 56 c,Translate 192 56 c,Translate 240 56 e, -- Linha 7
                                    Translate (-144) 64 e,Translate (-104) 64 e,Translate 416 64 c,Translate 376 64 e,Translate 352 64 c,Translate 280 64 c,Translate 264 64 e,Translate (-16) 64 c,Translate (-8) 64 e,Translate 80 64 e,Translate 240 64 c, -- Linha 8
                                    Translate (-128) 72 e,Translate 384 72 c,Translate 368 72 e,Translate 352 72 c,Translate 336 72 e,Translate 296 72 c,Translate 288 72 e,Translate (-40) 72 c,Translate 216 72 c,Translate 64 72 c,Translate 152 72 c,Translate 8 72 e,Translate 120 72 c, -- Linha 9
                                    Translate (-144) 80 e,Translate (-112) 80 e,Translate (-88) 80 e,Translate (-72) 80 e,Translate 416 80 e,Translate 384 80 c,Translate 320 80 c,Translate 280 80 c,Translate 256 80 c,Translate (-48) 80 e,Translate (-40) 80 e, -- Linha 10
                                    Translate (-160) 88 e,Translate (-144) 88 c,Translate (-128) 88 e,Translate (-88) 88 e,Translate 416 88 e,Translate 376 88 e, Translate 368 88 e,Translate 264 88 c,Translate (-48) 88 c,Translate (-40) 88 e,Translate 104 88 c,Translate 208 88 c,Translate 152 88 c, -- Linha 11
                                    Translate (-112) 96 e,Translate (-80) 96 e,Translate (-64) 96 c,Translate 352 96 e,Translate 344 96 e,Translate 336 96 c,Translate 328 96 e,Translate 312 96 e,Translate 264 96 e,Translate (-24) 96 c,Translate (-16) 96 e,Translate 0 96 c,Translate 112 96 e,Translate 96 96 e,Translate 208 96 e,Translate 88 96 e,Translate 48 96 c,Translate 128 96 e, -- Linha 12
                                    Translate (-144) 104 c,Translate (-136) 104 c,Translate (-104) 104 e, Translate (-64) 104 c,Translate 352 104 e,Translate 336 104 c,Translate 296 104 c,Translate 0 104 c,Translate 136 104 e,Translate 240 104 e,Translate 200 104 e,Translate 72 104 e, -- Linha 13
                                    Translate (-128) 112 c,Translate 384 112 c,Translate 312 112 c,Translate 280 112 e,Translate (-32) 112 c,Translate (-24) 112 c,Translate 112 112 e,Translate 16 112 e,Translate 8 112 c,Translate 192 112 e,Translate 136 112 e,Translate 176 112 e])] -- Linha 14
                                          where c = Color (greyN 0.15) (rectangleSolid 8 8)
                                                e = Color (greyN 0.2) (rectangleSolid 8 8)

-- | Desenha a borda do quadrado selecionado durante o jogo.

desenhaBorda :: Posicao -> Picture
desenhaBorda (x,y) = Pictures [Translate (x * 64 - 5) (-y * 64 + 86) (lineLoop [(-32,-32),(32,-32),(32,31),(-32,31)]),
                               Translate (x * 64 - 5) (-y * 64 + 86) (lineLoop [(-31,-31),(31,-31),(31,31),(-31,31)])]





-- {- Funções Relativas ao Mapa -} --

-- | Desenha o mapa do jogo.

desenhaMapa :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
               Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
               Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
               Picture -> Picture -> Picture -> Picture -> Mapa -> Tempo -> Picture
desenhaMapa texturaRelva tRE3 tRE6 tRE9 tRE12 tRE15 tRE18 tRE21
            texturaTerra tTE3 tTE6 tTE9 tTE12 tTE15 tTE18 tTE21
            texturaAgua tAE3 tAE6 tAE9 tAE12 tAE15 tAE18 tAE21
            texturaLava tLE15 tLE30 tLE45 tLE60 tLE75 tLE90 tLE105
            texturaLagrimasCristalinas tLCE3 tLCE6 tLCE9 tLCE12 tLCE15 tLCE18 tLCE21 mapa td
      = Pictures [desenhaTerras texturaTerra' (filtraPosicoesTerra mapa (0,0)),
                  desenhaRelvas texturaRelva' (filtraPosicoesRelva mapa (0,0)),
                  desenhaAguas texturaAgua' (filtraPosicoesAgua mapa (0,0)),
                  desenhaLavas texturaLava' (filtraPosicoesLava mapa (0,0)),
                  desenhaLagrimasCristalinas texturaLagrimasCristalinas' (filtraPosicoesLagrimasCristalinas mapa (0,0))]
                        where t = floor (td * 2)
                              (texturaRelva',texturaTerra',texturaAgua',texturaLava',texturaLagrimasCristalinas')
                                    | t <= 6  || t >= 42 = (tRE21,tTE21,tAE21,tLE105,tLCE21)
                                    | t == 7  || t == 41 = (tRE18,tTE18,tAE18,tLE90,tLCE18)
                                    | t == 8  || t == 40 = (tRE15,tTE15,tAE15,tLE75,tLCE15)
                                    | t == 9  || t == 39 = (tRE12,tTE12,tAE12,tLE60,tLCE12)
                                    | t == 10 || t == 38 = (tRE9,tTE9,tAE9,tLE45,tLCE9)
                                    | t == 11 || t == 37 = (tRE6,tTE6,tAE6,tLE30,tLCE6)
                                    | t == 12 || t == 36 = (tRE3,tTE3,tAE3,tLE15,tLCE3)
                                    | otherwise = (texturaRelva,texturaTerra,texturaAgua,texturaLava,texturaLagrimasCristalinas)

-- | Desenha um quadrado de terra.

desenhaTerras :: Picture -> [Posicao] -> Picture
desenhaTerras _ [] = Pictures []
desenhaTerras texturaTerra ((x,y):ps) = Pictures [Translate (x * 64 + 28) (-y * 64 + 28) texturaTerra,desenhaTerras texturaTerra ps]

-- | Desenha um quadrado de relva.

desenhaRelvas :: Picture -> [Posicao] -> Picture
desenhaRelvas _ [] = Pictures []
desenhaRelvas texturaRelva ((x,y):ps) = Pictures [Translate (x * 64 + 28) (-y * 64 + 28) texturaRelva,desenhaRelvas texturaRelva ps]

-- | Desenha um quadrado de água.

desenhaAguas :: Picture -> [Posicao] -> Picture
desenhaAguas _ [] = Pictures []
desenhaAguas texturaAgua ((x,y):ps) = Pictures [Translate (x * 64 + 28) (-y * 64 + 28) texturaAgua,desenhaAguas texturaAgua ps]

-- | Desenha um quadrado de lava.

desenhaLavas :: Picture -> [Posicao] -> Picture
desenhaLavas _ [] = Pictures []
desenhaLavas texturaLava ((x,y):ps) = Pictures [Translate (x * 64 + 28) (-y * 64 + 28) texturaLava,desenhaLavas texturaLava ps]

-- | Desenha um quadrado de lágrimas cristalinas.

desenhaLagrimasCristalinas :: Picture -> [Posicao] -> Picture
desenhaLagrimasCristalinas _ [] = Pictures []
desenhaLagrimasCristalinas texturaLagrimasCristalinas ((x,y):ps) = Pictures [Translate (x * 64 + 28) (-y * 64 + 28) texturaLagrimasCristalinas,desenhaLagrimasCristalinas texturaLagrimasCristalinas ps]





-- {- Funções Relativas às Torres -} --

-- | Desenha as torres do jogo.

desenhaTorres :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> [Torre] -> Picture
desenhaTorres _ _ _ _ _ _ _ _ _ _ _ [] = Pictures []
desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
              imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado
              imagem1 imagem2 imagem3 (t:ts)
      | n == 1 = Pictures [desenhaArqueiroFlamejante imagemArqueiroFlamejante imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 2 = Pictures [desenhaSabioCorruptor imagemSabioCorruptor imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 3 = Pictures [desenhaCarvalhoEnt imagemCarvalhoEnt imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 4 = Pictures [desenhaMaquinaDeResina imagemMaquinaDeResina imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 5 = Pictures [desenhaMagoDeGelo imagemMagoDeGelo imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 6 = Pictures [desenhaFornalhaArdente imagemFornalhaArdente imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 7 = Pictures [desenhaCanhao imagemCanhao imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | n == 8 = Pictures [desenhaJuizGelado imagemJuizGelado imagemNivel (posicaoTorre t),
                           desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                           imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts]
      | otherwise = desenhaTorres imagemArqueiroFlamejante imagemSabioCorruptor imagemCarvalhoEnt imagemMaquinaDeResina 
                    imagemMagoDeGelo imagemFornalhaArdente imagemCanhao imagemJuizGelado imagem1 imagem2 imagem3 ts
                        where n = numeroTorre t
                              m = nivelTorre t
                              imagemNivel
                                    | m == 1 = Translate 24 24 (Scale 0.25 0.25 imagem1)
                                    | m == 2 = Translate 24 24 (Scale 0.25 0.25 imagem2)
                                    | otherwise = Translate 24 24 (Scale 0.25 0.25 imagem3)

-- | Desenha a torre 'Arqueiro Flamejante'.

desenhaArqueiroFlamejante :: Picture -> Picture -> Posicao -> Picture
desenhaArqueiroFlamejante imagemArqueiroFlamejante imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemArqueiroFlamejante,
                                                                                 Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Sábio Corruptor'.

desenhaSabioCorruptor :: Picture -> Picture -> Posicao -> Picture
desenhaSabioCorruptor imagemSabioCorruptor imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemSabioCorruptor,
                                                                         Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Carvalho Ent'.

desenhaCarvalhoEnt :: Picture -> Picture -> Posicao -> Picture
desenhaCarvalhoEnt imagemCarvalhoEnt imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemCarvalhoEnt,
                                                                   Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Morcego Infernal'.

desenhaMaquinaDeResina :: Picture -> Picture -> Posicao -> Picture
desenhaMaquinaDeResina imagemMaquinaDeResina imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemMaquinaDeResina,
                                                                           Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Mago de Gelo'.

desenhaMagoDeGelo :: Picture -> Picture -> Posicao -> Picture
desenhaMagoDeGelo imagemMagoDeGelo imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemMagoDeGelo,
                                                                 Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Fornalha Ardente'.

desenhaFornalhaArdente :: Picture -> Picture -> Posicao -> Picture
desenhaFornalhaArdente imagemFornalhaArdente imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemFornalhaArdente,
                                                                           Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Canhão'.

desenhaCanhao :: Picture -> Picture -> Posicao -> Picture
desenhaCanhao imagemCanhao imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemCanhao,
                                                         Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha a torre 'Juiz Gelado'.

desenhaJuizGelado :: Picture -> Picture -> Posicao -> Picture
desenhaJuizGelado imagemJuizGelado imagemNivel (x,y) = Pictures [Translate (x * 64 - 4) (-y * 64 + 60) imagemJuizGelado,
                                                                 Translate (x * 64 - 4) (-y * 64 + 60) imagemNivel]

-- | Desenha o alcance de uma torre.

desenhaAlcanceTorre :: Bool -> Posicao -> [Torre] -> Picture
desenhaAlcanceTorre False _ _ = Pictures []
desenhaAlcanceTorre _ _ [] = Pictures []
desenhaAlcanceTorre b (x,y) (t:ts)
      | posicaoTorre t == (x,y) = Translate (x * 64 - 928) (-y * 64 + 391) (Color black (circle (64 * alcanceTorre t)))
      | otherwise = desenhaAlcanceTorre b (x,y) ts





-- {- Função Relativa à Base -} --

desenhaBase :: Base -> Picture -> Picture
desenhaBase (Base _ (x,y) _) imagemBase = Translate (x * 64 - 4) (-y * 64 + 64) (Scale (1 / 6) (1 / 6) imagemBase)





-- {- Função Relativa aos Portais -} --

desenhaPortal :: Picture -> Portal -> Picture
desenhaPortal imagemPortal (Portal (x,y) _) = Translate (-924) 443 (Translate (x * 64 - 4) (-y * 64 + 60) (Scale (1 / 5.625) (1 / 5.625) imagemPortal))





-- {- Funções Relativas aos Inimigos -} --

-- | Desenha um dado inimigo genérico (pré-definido).

desenhaInimigo :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture ->
                  Picture -> Picture -> Picture -> Picture -> Inimigo -> Picture
desenhaInimigo imagemGharzul imagemZalrok imagemZalrokReforcado imagemFenrir imagemTharok imagemAranthisMortifera imagemDragaoDeRocha imagemFenrirSulfr
      imagemOnyc imagemMonstroDeMagma imagemReaper imagemIgnaroth imagemDragaoCristalino imagemLeaoCristalino imagemFungalith imagemAethralith
      (Inimigo (x,y) _ vida _ _ _ projeteis _ vidaMax n)
      | n == 1 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaGharzul imagemGharzul (x,y) vida vidaMax])
      | n == 2 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaZalrok imagemZalrok (x,y) vida vidaMax])
      | n == 3 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaFenrir imagemFenrir (x,y) vida vidaMax])
      | n == 4 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaTharok imagemTharok (x,y) vida vidaMax])
      | n == 5 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaAranthisMortifera imagemAranthisMortifera (x,y) vida vidaMax])
      | n == 6 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaZalrokReforcado imagemZalrokReforcado (x,y) vida vidaMax])
      | n == 7 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaDragaoDeRocha imagemDragaoDeRocha (x,y) vida vidaMax])
      | n == 8 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaFenrirSulfr imagemFenrirSulfr (x,y) vida vidaMax])
      | n == 9 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaOnyc imagemOnyc (x,y) vida vidaMax])
      | n == 10 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaMonstroDeMagma imagemMonstroDeMagma (x,y) vida vidaMax])
      | n == 11 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaReaper imagemReaper (x,y) vida vidaMax])
      | n == 12 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaIgnaroth imagemIgnaroth (x,y) vida vidaMax])
      | n == 13 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaDragaoCristalino imagemDragaoCristalino (x,y) vida vidaMax])
      | n == 14 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaLeaoCristalino imagemLeaoCristalino (x,y) vida vidaMax])
      | n == 15 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaFungalith imagemFungalith (x,y) vida vidaMax])
      | n == 16 = Translate (-926) 506 (Pictures [Translate (x * 64) (-y * 64) (desenhaEfeitoProjeteis projeteis),desenhaAethralith imagemAethralith (x,y) vida vidaMax])

-- | Desenha o efeito dos projéteis de um inimigo.

desenhaEfeitoProjeteis :: [Projetil] -> Picture
desenhaEfeitoProjeteis projeteis = case projeteis of
      [] -> Pictures []
      [Projetil Fogo _] -> Color corFogo (circleSolid 12)
            where corFogo = makeColor 1 (96 / 255) 0 0.7
      [Projetil Gelo _] -> Color corGelo (circleSolid 12)
            where corGelo = makeColor 0 (187 / 255) 1 0.6
      [Projetil Resina _] -> Color corResina (circleSolid 12)
            where corResina = makeColor (138 / 255) (70 / 255) 0 0.7
      [Projetil Fraqueza _] -> Color corFraqueza (circleSolid 12)
            where corFraqueza = makeColor 1 0 (153 / 255) 0.7
      ps -> if any verifFogo ps then Color corFogoFraqueza (circleSolid 12)
            else if any verifGelo ps && any verifFraqueza ps && any verifResina ps then Color corGeloFraquezaResina (circleSolid 12)
            else if any verifGelo ps && any verifFraqueza ps then Color corGeloFraqueza (circleSolid 12)
            else if any verifGelo ps then Color corGeloResina (circleSolid 12)
            else Color corResinaFraqueza (circleSolid 12)
                  where corFogoFraqueza = makeColor 1 0 0 0.7
                        corGeloFraquezaResina = makeColor 1 (15 / 255) 0 0.7
                        corGeloFraqueza = makeColor 1 (94 / 255) 1 0.7
                        corGeloResina = makeColor (179 / 255) (100 / 255) (70 / 255) 0.7
                        corResinaFraqueza = makeColor (182 / 255) (57 / 255) (26 / 255) 0.9
      
-- | Desenha um 'Gharzul'.

desenhaGharzul :: Picture -> Posicao -> Float -> Float -> Picture
desenhaGharzul imagemGharzul (x,y) v vMax = Translate (x * 64) (-y * 64) (Pictures [imagemGharzul,Translate 0 20 (Color black (rectangleSolid 20 3)),
                                                                      desenhaVida v vMax])

-- | Desenha um 'Zalrok'.

desenhaZalrok :: Picture -> Posicao -> Float -> Float -> Picture
desenhaZalrok imagemZalrok (x,y) v vMax = Translate (x * 64) (-y * 64) (Pictures [imagemZalrok,Translate 0 20 (Color black (rectangleSolid 20 3)),
                                                                      desenhaVida v vMax])

-- | Desenha um 'Fenrir'.

desenhaFenrir :: Picture -> Posicao -> Float -> Float -> Picture
desenhaFenrir imagemFenrir (x,y) v vMax = Translate (x * 64) (-y * 64) (Pictures [imagemFenrir,Translate 0 20 (Color black (rectangleSolid 20 3)),
                                                                      desenhaVida v vMax])

-- | Desenha um 'Tharok'.

desenhaTharok :: Picture -> Posicao -> Float -> Float -> Picture
desenhaTharok imagemTharok (x,y) v vMax = Translate (x * 64) (-y * 64) (Pictures [imagemTharok,Translate 0 20 (Color black (rectangleSolid 20 3)),
                                                                      desenhaVida v vMax])

-- | Desenha um 'Aranthis Mortifera'.

desenhaAranthisMortifera :: Picture -> Posicao -> Float -> Float -> Picture
desenhaAranthisMortifera imagemAranthisMortifera (x,y) v vMax = Translate (x * 64) (-y * 64)
      (Pictures [imagemAranthisMortifera,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Zalrok Reforçado'.

desenhaZalrokReforcado :: Picture -> Posicao -> Float -> Float -> Picture
desenhaZalrokReforcado imagemZalrokReforcado (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemZalrokReforcado,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Dragão de Rocha'.

desenhaDragaoDeRocha :: Picture -> Posicao -> Float -> Float -> Picture
desenhaDragaoDeRocha imagemDragaoDeRocha (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemDragaoDeRocha,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'FenrirSulfr'.

desenhaFenrirSulfr :: Picture -> Posicao -> Float -> Float -> Picture
desenhaFenrirSulfr imagemFenrirSulfr (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemFenrirSulfr,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Onyc'.

desenhaOnyc :: Picture -> Posicao -> Float -> Float -> Picture
desenhaOnyc imagemOnyc (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemOnyc,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Montro de  Magma'.

desenhaMonstroDeMagma :: Picture -> Posicao -> Float -> Float -> Picture
desenhaMonstroDeMagma imagemMonstroDeMagma (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemMonstroDeMagma,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Reaper'.

desenhaReaper :: Picture -> Posicao -> Float -> Float -> Picture
desenhaReaper imagemReaper (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemReaper,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Ignaroth'.

desenhaIgnaroth :: Picture -> Posicao -> Float -> Float -> Picture
desenhaIgnaroth imagemIgnaroth (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemIgnaroth,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Dragão Cristalino'.

desenhaDragaoCristalino :: Picture -> Posicao -> Float -> Float -> Picture
desenhaDragaoCristalino imagemDragaoCristalino (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemDragaoCristalino,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Leão Cristalino'.

desenhaLeaoCristalino :: Picture -> Posicao -> Float -> Float -> Picture
desenhaLeaoCristalino imagemLeaoCristalino (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemLeaoCristalino,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Fungalith'.

desenhaFungalith :: Picture -> Posicao -> Float -> Float -> Picture
desenhaFungalith imagemFungalith (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemFungalith,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha um 'Aethralith'.

desenhaAethralith :: Picture -> Posicao -> Float -> Float -> Picture
desenhaAethralith imagemAethralith (x,y) v vMax = Translate (x * 64) (-y * 64) 
      (Pictures [imagemAethralith,Translate 0 20 (Color black (rectangleSolid 20 3)),desenhaVida v vMax])

-- | Desenha a (parte vermelha da) barra de vida de um inimigo.

desenhaVida :: Float -> Float -> Picture
desenhaVida v vMax = Pictures [Translate 0 20 (Color red (rectangleSolid (20 * v / vMax) 3))]