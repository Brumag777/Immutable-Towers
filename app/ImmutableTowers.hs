module ImmutableTowers where

import LI12425

data Tema = Natural | Flamejante | Cristalino deriving (Show,Eq)

type InimigosDerrotados = Int

type ConquistasConcluidas = [Int] -- Não inclui as conquistas 1, 2 e 3 pois são verificadas através do type 'InimigosDerrotados'.

type NiveisConcluidos = [Int]

type VisibilidadeAlcance = Bool

type NumeroPagina = Int

data ImmutableTowers = MenuPrincipal    InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo |
                       Conquistas       InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo | 
                       SelecaoNiveis    InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo |
                       JogoACorrer Jogo InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo | 
                       JogoPausado Jogo InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo | 
                       PerdeuJogo  Jogo InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo |
                       GanhouJogo  Jogo InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo | 
                       Temas            InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo | 
                       InfoTorres       InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance (Maybe Jogo) Tempo |
                       InfoInimigos     InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance (Maybe Jogo) NumeroPagina Tempo |
                       InfoJogo         InimigosDerrotados ConquistasConcluidas NiveisConcluidos Tema VisibilidadeAlcance Tempo NumeroPagina
  




-- {- Funções Auxiliares -} --

aumentaVidaInimigos :: [Portal] -> [Portal]
aumentaVidaInimigos = map aumentaVidaInimigosAux

aumentaVidaInimigosAux :: Portal -> Portal
aumentaVidaInimigosAux (Portal posicao ondas) = Portal posicao (map aumentaVidaInimigosAux' ondas)

aumentaVidaInimigosAux' :: Onda -> Onda
aumentaVidaInimigosAux' (Onda inimigos ciclo tempo entrada) = Onda (map aumentaVidaInimigosAux'' inimigos) ciclo tempo entrada

aumentaVidaInimigosAux'' :: Inimigo -> Inimigo
aumentaVidaInimigosAux'' (Inimigo posicao direcao vida velocidade ataque butim projeteis imunidades vidMax num) = Inimigo posicao direcao (fromInteger (floor (vida * 1.2))) velocidade ataque butim projeteis imunidades (fromInteger (floor (vidMax * 1.2))) num





-- | Estado inicial do jogo.

estadoInicial :: ImmutableTowers
estadoInicial = MenuPrincipal 0 [] [] Natural False 0





-- {- Nível 1 - Planícies Verdejantes -} --

planiciesVerdejantes :: Jogo
planiciesVerdejantes = Jogo basePlaniciesVerdejantes [portalPlaniciesVerdejantes1,portalPlaniciesVerdejantes2]
                       [] mapaPlaniciesVerdejantes [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina] 1 (10.5,6.5) 0 (False,0,[]) 0

planiciesVerdejantesElite :: Jogo
planiciesVerdejantesElite = Jogo basePlaniciesVerdejantes (aumentaVidaInimigos [portalPlaniciesVerdejantes1,portalPlaniciesVerdejantes2])
                       [] mapaPlaniciesVerdejantes [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina] 5 (10.5,6.5) 0 (False,0,[]) 0

portalPlaniciesVerdejantes1 :: Portal
portalPlaniciesVerdejantes1 = Portal (1.5,1.5) [onda1,onda2,onda5,onda6]
  where onda1 = Onda [gharzul,gharzul,gharzul,gharzul] 5 0 10
        onda2 = Onda [gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul] 2.5 0 30
        onda5 = Onda [zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok] 3 0 150
        onda6 = Onda [zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,zalrok,tharok] 3 0 185

portalPlaniciesVerdejantes2 :: Portal
portalPlaniciesVerdejantes2 = Portal (17.5,8.5) [onda3,onda4,onda5,onda6]
  where onda3 = Onda [fenrir,fenrir,fenrir,fenrir,fenrir,fenrir] 8 0 50
        onda4 = Onda [fenrir,gharzul,gharzul,fenrir,gharzul,gharzul,fenrir,gharzul,gharzul,fenrir,gharzul,gharzul] 4 0 100
        onda5 = Onda [fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir] 3 0 150
        onda6 = Onda [fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,tharok] 3 0 185

arqueiroFlamejante :: (Creditos,Torre)
arqueiroFlamejante = (100,Torre (0,0) 16 2 1 1.2 1.2 (Projetil Fogo (Finita 3)) 20 20 1 1)

arqueiroFlamejante2 :: Torre
arqueiroFlamejante2 = Torre (0,0) 24 2.5 1 1.1 1 (Projetil Fogo (Finita 4)) 20 20 2 1

arqueiroFlamejante3 :: Torre
arqueiroFlamejante3 = Torre (0,0) 32 3 1 1 0.8 (Projetil Fogo (Finita 5)) 20 20 3 1

sabioCorruptor :: (Creditos,Torre)
sabioCorruptor = (200,Torre (0,0) 20 2.5 1 4 4 (Projetil Fraqueza (Finita 4)) 0 30 1 2)

sabioCorruptor2 :: Torre
sabioCorruptor2 = Torre (0,0) 40 3 1 3.5 3.5 (Projetil Fraqueza (Finita 8)) 0 30 2 2

sabioCorruptor3 :: Torre
sabioCorruptor3 = Torre (0,0) 60 3.5 1 3 3 (Projetil Fraqueza (Finita 12)) 0 30 3 2

carvalhoEnt :: (Creditos,Torre)
carvalhoEnt = (200,Torre (0,0) 5 1.5 64 0.5 0.5 (Projetil Resina (Finita 3)) 0 0 1 3)

carvalhoEnt2 :: Torre
carvalhoEnt2 = Torre (0,0) 7.5 1.75 64 0.5 0.45 (Projetil Resina (Finita 3.5)) 0 0 2 3

carvalhoEnt3 :: Torre
carvalhoEnt3 = Torre (0,0) 10 2 64 0.5 0.4 (Projetil Resina (Finita 4)) 0 0 3 3

maquinaDeResina :: (Creditos,Torre)
maquinaDeResina = (160,Torre (0,0) 20 2 1 2.4 2 (Projetil Resina Infinita) 15 15 1 4)

maquinaDeResina2 :: Torre
maquinaDeResina2 = Torre (0,0) 25 2.5 2 2 1.6 (Projetil Resina Infinita) 15 15 2 4

maquinaDeResina3 :: Torre
maquinaDeResina3 = Torre (0,0) 30 3 3 1.6 1.2 (Projetil Resina Infinita) 15 15 3 4

mapaPlaniciesVerdejantes :: Mapa
mapaPlaniciesVerdejantes = [[r,r,r,r,r,r,r,a,a,a,a,a,a,a,a,a,a,a,a,a,a],
                            [r,p,t,r,r,r,r,r,r,r,r,a,a,a,a,a,a,a,a,a,a],
                            [r,r,t,t,t,t,r,r,r,r,r,r,r,r,r,a,a,a,a,a,a],
                            [r,r,r,r,r,t,t,t,r,r,r,r,r,t,t,t,r,r,a,a,a],
                            [r,r,t,t,t,t,r,t,t,r,r,b,t,t,r,r,r,r,r,a,a],
                            [t,t,t,r,r,r,r,r,t,t,t,t,r,r,r,r,r,r,r,a,a],
                            [r,r,r,r,r,r,r,r,r,r,r,t,t,r,r,r,r,r,r,a,a],
                            [r,r,a,a,a,a,r,r,r,r,r,r,t,t,t,t,r,r,r,r,a],
                            [a,a,a,a,a,a,a,a,a,r,r,r,r,r,r,t,t,p,r,r,a],
                            [a,a,a,r,r,a,a,a,a,a,a,r,r,r,r,r,r,r,r,a,a],
                            [a,a,a,r,r,r,r,a,a,a,a,a,a,a,r,r,r,a,a,a,a],
                            [a,a,a,a,r,r,r,a,a,a,a,a,a,a,a,a,a,a,a,a,a],
                            [r,r,a,a,a,a,a,a,a,a,a,a,r,r,r,a,a,a,a,a,a],
                            [r,r,r,r,a,a,a,a,a,a,r,r,r,r,r,r,r,a,a,a,a]]
                              where p = Terra
                                    b = Terra
                                    t = Terra
                                    r = Relva
                                    a = Agua

basePlaniciesVerdejantes :: Base
basePlaniciesVerdejantes = Base 100 (11.5,4.5) 400

-- | Um monstro humanoide de tamanho reduzido.

gharzul :: Inimigo
gharzul = Inimigo (0,0) Norte 50 0.5 10 10 [] [] 50 1

-- | Um montro humanoide.

zalrok :: Inimigo
zalrok = Inimigo (0,0) Norte 150 0.4 20 20 [] [] 150 2

-- | Um lobo.

fenrir :: Inimigo
fenrir = Inimigo (0,0) Norte 75 0.8 10 25 [] [Resina,Gelo] 75 3

-- | Um Mini-Boss montro humanoide.

tharok :: Inimigo
tharok = Inimigo (0,0) Norte 600 0.5 100 100 [] [Gelo] 600 4





-- {- Nível 2 - Floresta da Morte -} --

florestaDaMorte :: Jogo
florestaDaMorte = Jogo baseFlorestaDaMorte [portalFlorestaDaMorte1,portalFlorestaDaMorte2,portalFlorestaDaMorte3,portalFlorestaDaMorte4]
                       [] mapaFlorestaDaMorte [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente] 2 (10.5,6.5) 0 (False,0,[]) 0

florestaDaMorteElite :: Jogo
florestaDaMorteElite = Jogo baseFlorestaDaMorte (aumentaVidaInimigos [portalFlorestaDaMorte1,portalFlorestaDaMorte2,portalFlorestaDaMorte3,portalFlorestaDaMorte4])
                       [] mapaFlorestaDaMorte [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente] 6 (10.5,6.5) 0 (False,0,[]) 0

portalFlorestaDaMorte1 :: Portal
portalFlorestaDaMorte1 = Portal (0.5,1.5) [onda1,onda3,onda7,onda8]
      where onda1 = Onda [fenrir,fenrir,zalrok,zalrok,fenrir,fenrir,zalrok,zalrok,fenrir,fenrir,zalrok,zalrok,fenrir,fenrir,zalrok,zalrok] 2 0 10
            onda3 = Onda [zalrok,zalrokReforcado,zalrok,zalrokReforcado,zalrok,zalrokReforcado,zalrok,zalrokReforcado] 4 0 80
            onda7 = Onda [zalrokReforcado,zalrokReforcado,tharok,zalrokReforcado,zalrokReforcado,tharok,zalrokReforcado,zalrokReforcado,tharok,
                          zalrokReforcado,zalrokReforcado,tharok,zalrokReforcado,zalrokReforcado,tharok] 4 0 220
            onda8 = Onda [fenrirSulfr,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrirSulfr] 2 0 300

portalFlorestaDaMorte2 :: Portal
portalFlorestaDaMorte2 = Portal (20.5,3.5) [onda2,onda3,onda6,onda7,onda8]
      where onda2 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera] 3 0 50
            onda3 = Onda [zalrok,zalrokReforcado,zalrok,zalrokReforcado,zalrok,zalrokReforcado,zalrok,zalrokReforcado] 4 0 80
            onda6 = Onda [aranthisMortifera,aranthisMortifera,dragaoDeRocha,aranthisMortifera,aranthisMortifera,dragaoDeRocha,aranthisMortifera,aranthisMortifera,
                          dragaoDeRocha,aranthisMortifera,aranthisMortifera,dragaoDeRocha] 2 0 185
            onda7 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera] 2 0 220
            onda8 = Onda [fenrirSulfr,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrirSulfr] 2 0 300

portalFlorestaDaMorte3 :: Portal
portalFlorestaDaMorte3 = Portal (1.5,10.5) [onda4,onda5,onda7,onda8]
      where onda4 = Onda [gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,
                          gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,
                          gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul,gharzul] 1 0 120
            onda5 = Onda [dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha] 6 0 155
            onda7 = Onda [dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,
                          dragaoDeRocha] 6 0 220
            onda8 = Onda [fenrirSulfr,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrirSulfr] 2 0 300

portalFlorestaDaMorte4 :: Portal
portalFlorestaDaMorte4 = Portal (8.5,11.5) [onda4,onda5,onda7,onda8]
      where onda4 = Onda [fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir] 1 0 120
            onda5 = Onda [zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado] 3 0 155
            onda7 = Onda [dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,
                          dragaoDeRocha] 6 0 220
            onda8 = Onda [fenrirSulfr,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,
                          fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrir,fenrirSulfr] 2 0 300

magoDeGelo :: (Creditos,Torre)
magoDeGelo = (80,Torre (0,0) 8 2 1 1.5 2 (Projetil Gelo (Finita 1)) 0 20 1 5)

magoDeGelo2 :: Torre
magoDeGelo2 = Torre (0,0) 12 2.25 1 1.5 1.9 (Projetil Gelo (Finita 1.1)) 0 20 2 5

magoDeGelo3 :: Torre
magoDeGelo3 = Torre (0,0) 16 2.5 1 1.5 1.8 (Projetil Gelo (Finita 1.2)) 0 20 3 5

fornalhaArdente :: (Creditos,Torre)
fornalhaArdente = (240,Torre (0,0) 20 2 64 6 6 (Projetil Fogo Infinita) 0 25 1 6)

fornalhaArdente2 :: Torre
fornalhaArdente2 = Torre (0,0) 40 2.5 64 5.25 5.25 (Projetil Fogo Infinita) 0 25 2 6

fornalhaArdente3 :: Torre
fornalhaArdente3 = Torre (0,0) 60 3 64 4.5 4.5 (Projetil Fogo Infinita) 0 25 3 6

mapaFlorestaDaMorte :: Mapa
mapaFlorestaDaMorte = [[l,l,l,l,l,l,l,r,r,r,r,r,r,r,r,a,a,r,r,r,r],
                       [p,t,t,l,l,r,r,r,r,r,r,r,r,r,a,a,a,r,r,r,r],
                       [l,l,t,t,t,r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r],
                       [l,r,r,r,t,t,t,t,r,r,r,r,r,a,a,a,r,r,r,r,p],
                       [r,r,r,r,r,r,r,t,t,r,r,r,a,a,a,a,a,r,r,r,t],
                       [r,r,r,r,r,r,r,r,t,r,r,a,a,r,r,r,a,a,r,r,t],
                       [r,r,r,r,r,r,r,r,t,r,r,a,r,r,r,r,r,a,r,t,t],
                       [r,r,t,t,t,t,t,t,t,t,t,t,t,t,b,t,t,t,t,t,r],
                       [r,t,t,r,t,r,r,r,r,r,r,a,r,r,r,r,r,a,r,r,r],
                       [r,t,r,r,t,r,r,r,r,r,r,a,a,r,r,r,a,a,r,r,r],
                       [r,p,r,r,t,t,r,r,r,r,r,r,a,a,a,a,a,r,r,r,r],
                       [r,r,r,r,r,t,t,t,p,r,r,r,r,a,a,a,r,r,r,r,r],
                       [r,r,r,r,r,r,r,r,r,r,r,r,r,a,a,r,r,r,r,r,r],
                       [r,r,r,r,r,r,r,r,r,r,r,r,a,a,a,r,r,r,r,r,r]]
                        where p = Terra
                              b = Terra
                              t = Terra
                              r = Relva
                              a = Agua
                              l = Lava

baseFlorestaDaMorte :: Base
baseFlorestaDaMorte = Base 100 (14.5,7.5) 800

-- | Uma aranha gigante.

aranthisMortifera :: Inimigo
aranthisMortifera = Inimigo (0,0) Norte 150 0.5 20 20 [] [Fraqueza] 150 5

-- | Um montro humanoide.

zalrokReforcado :: Inimigo
zalrokReforcado = Inimigo (0,0) Norte 320 0.4 30 40 [] [] 320 6

-- | Um dragão de rocha.

dragaoDeRocha :: Inimigo
dragaoDeRocha = Inimigo (0,0) Norte 700 0.5 40 80 [] [] 700 7

-- | Um Mini-Boss lobo.

fenrirSulfr :: Inimigo
fenrirSulfr = Inimigo (0,0) Norte 1200 0.7 100 100 [] [Gelo,Resina] 1200 8





-- {- Nível 3 - Vulcão das Trevas -} --

vulcaoDasTrevas :: Jogo
vulcaoDasTrevas = Jogo baseVulcaoDasTrevas [portalVulcaoDasTrevas1,portalVulcaoDasTrevas2,portalVulcaoDasTrevas3,portalVulcaoDasTrevas4]
                       [] mapaVulcaoDasTrevas [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente,canhao,juizGelado] 3 (10.5,6.5) 0 (False,0,[]) 0

vulcaoDasTrevasElite :: Jogo
vulcaoDasTrevasElite = Jogo baseVulcaoDasTrevas (aumentaVidaInimigos [portalVulcaoDasTrevas1,portalVulcaoDasTrevas2,portalVulcaoDasTrevas3,portalVulcaoDasTrevas4])
                       [] mapaVulcaoDasTrevas [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente,canhao,juizGelado] 7 (10.5,6.5) 0 (False,0,[]) 0

portalVulcaoDasTrevas1 :: Portal
portalVulcaoDasTrevas1 = Portal (12.5,1.5) [onda1,onda3,onda5,onda6,onda8,onda10]
      where onda1 = Onda [onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc] 4 0 10
            onda3 = Onda [onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper] 2 0 100
            onda5 = Onda [onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc] 1.5 0 190
            onda6 = Onda [onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc,
                          onyc,onyc,onyc,onyc,onyc,onyc,onyc,onyc] 2 0 225
            onda8 = Onda [onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper] 1.5 0 350
            onda10 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,
                           montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 440

portalVulcaoDasTrevas2 :: Portal
portalVulcaoDasTrevas2 = Portal (15.5,2.5) [onda1,onda3,onda5,onda6,onda8,onda10]
      where onda1 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera] 4 0 10
            onda3 = Onda [onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper] 2 0 100
            onda5 = Onda [reaper,reaper,reaper,reaper] 6 0 190
            onda6 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera] 2 0 225
            onda8 = Onda [onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper,onyc,onyc,onyc,onyc,reaper] 1.5 0 350
            onda10 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,
                           montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 440

portalVulcaoDasTrevas3 :: Portal
portalVulcaoDasTrevas3 = Portal (8.5,8.5) [onda2,onda4,onda5,onda7,onda9,onda10]
      where onda2 = Onda [montroDeMagma,montroDeMagma] 20 0 50
            onda4 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 140
            onda5 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 10 0 190
            onda7 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 300
            onda9 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,
                          montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 2.5 0 390
            onda10 = Onda [ignaroth,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,
                           montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 440

portalVulcaoDasTrevas4 :: Portal
portalVulcaoDasTrevas4 = Portal (9.5,13.5) [onda2,onda4,onda5,onda7,onda9,onda10]
      where onda2 = Onda [zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,
                          zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado] 2.5 0 50
            onda4 = Onda [onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper] 3.3 0 140
            onda5 = Onda [dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha] 3 0 190
            onda7 = Onda [zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,
                          zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,
                          zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,
                          zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado,zalrokReforcado] 1.25 0 300
            onda9 = Onda [onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,reaper,onyc,onyc,
                          reaper] 1.7 0 390
            onda10 = Onda [montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,
                           montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma,montroDeMagma] 5 0 440

canhao :: (Creditos,Torre)
canhao = (240,Torre (0,0) 100 2 1 3 3.5 (Projetil Fraqueza (Finita 2.5)) 5 20 1 7)

canhao2 :: Torre
canhao2 = Torre (0,0) 150 2.5 1 4 3 (Projetil Fraqueza (Finita 3)) 5 20 2 7

canhao3 :: Torre
canhao3 = Torre (0,0) 200 3 1 4 2.5 (Projetil Fraqueza (Finita 3.5)) 5 20 3 7

juizGelado :: (Creditos,Torre)
juizGelado = (250,Torre (0,0) 150 1.5 1 4 4 (Projetil Gelo (Finita 1)) 0 15 1 8)

juizGelado2:: Torre
juizGelado2 = Torre (0,0) 225 1.75 1 3.75 3.5 (Projetil Gelo (Finita 1.5)) 0 15 2 8

juizGelado3 :: Torre
juizGelado3 = Torre (0,0) 300 2 1 3.5 3 (Projetil Gelo (Finita 2)) 0 15 3 8

mapaVulcaoDasTrevas :: Mapa
mapaVulcaoDasTrevas = [[t,t,l,l,t,r,t,l,l,t,t,r,r,r,r,r,r,t,l,l,l],
                       [t,t,l,l,t,r,t,l,l,t,r,r,p,r,r,r,r,t,l,l,l],
                       [t,l,l,t,t,t,t,l,t,t,r,r,t,r,t,p,r,t,t,l,l],
                       [t,l,l,t,t,t,l,l,t,t,r,r,t,t,t,r,r,t,t,l,l],
                       [l,l,l,l,t,t,l,l,t,t,r,r,t,r,r,r,r,t,l,l,l],
                       [l,l,l,l,t,l,l,t,t,t,r,r,t,r,r,r,r,t,l,l,l],
                       [l,l,l,l,l,l,l,t,t,r,r,r,t,t,r,r,r,t,l,l,l],
                       [t,l,l,l,l,l,l,l,t,r,r,r,r,t,t,r,r,t,t,l,l],
                       [t,t,l,l,l,l,l,l,p,t,t,r,r,r,t,t,r,t,t,l,l],
                       [r,t,t,t,l,l,l,t,t,r,t,t,r,r,r,t,r,r,t,t,l],
                       [r,r,r,t,t,l,t,t,t,r,r,t,t,t,t,t,r,r,r,t,t],
                       [r,r,r,t,t,l,t,t,r,r,r,r,r,r,r,t,r,r,r,r,r],
                       [r,r,r,t,l,l,t,t,r,r,t,t,t,t,t,t,t,t,t,b,r],
                       [r,r,t,t,l,t,t,r,r,p,t,r,r,r,r,r,r,r,r,r,r]]
                        where p = Terra
                              b = Terra
                              t = Terra
                              r = Relva
                              l = Lava

baseVulcaoDasTrevas :: Base
baseVulcaoDasTrevas = Base 100 (19.5,12.5) 1500

-- | Um morcego.

onyc :: Inimigo
onyc = Inimigo (0,0) Norte 150 1 20 30 [] [] 150 9

-- | Um montro de lava.

montroDeMagma :: Inimigo
montroDeMagma = Inimigo (0,0) Norte 2400 0.35 40 80 [] [Fogo] 2400 10

-- | Um montro.

reaper :: Inimigo
reaper = Inimigo (0,0) Norte 900 0.5 30 75 [] [Fraqueza] 900 11

-- | Um Boss montro de fogo.

ignaroth :: Inimigo
ignaroth = Inimigo (0,0) Norte 20000 0.35 100 600 [] [Gelo,Fogo] 20000 12





-- {- Nível Secreto - Ilhas Cristalinas -} --

ilhasCristalinas :: Jogo
ilhasCristalinas = Jogo baseIlhasCristalinas [portalIlhasCristalinas1,portalIlhasCristalinas2,portalIlhasCristalinas3,portalIlhasCristalinas4]
                       [] mapaIlhasCristalinas [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente,canhao,juizGelado] 4 (10.5,6.5) 0 (False,0,[]) 0

ilhasCristalinasElite :: Jogo
ilhasCristalinasElite = Jogo baseIlhasCristalinas (aumentaVidaInimigos [portalIlhasCristalinas1,portalIlhasCristalinas2,portalIlhasCristalinas3,portalIlhasCristalinas4])
                       [] mapaIlhasCristalinas [] [arqueiroFlamejante,sabioCorruptor,carvalhoEnt,maquinaDeResina,magoDeGelo,fornalhaArdente,canhao,juizGelado] 8 (10.5,6.5) 0 (False,0,[]) 0

portalIlhasCristalinas1 :: Portal
portalIlhasCristalinas1 = Portal (11.5,9.5) [onda1,onda3,onda5,onda6,onda8,onda9]
      where onda1 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera] 3 0 10
            onda3 = Onda [dragaoDeRocha,dragaoDeRocha,dragaoDeRocha,dragaoDeRocha] 16 0 70
            onda5 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,
                          leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino] 6 0 170
            onda6 = Onda [fungalith] 10 0 230
            onda8 = Onda [dragaoCristalino,dragaoCristalino,fungalith,dragaoCristalino,dragaoCristalino,fungalith] 5 0 320
            onda9 = Onda [leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,
                          leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith] 5 0 385

portalIlhasCristalinas2 :: Portal
portalIlhasCristalinas2 = Portal (1.5,5.5) [onda2,onda3,onda5,onda6,onda8,onda9]
      where onda2 = Onda [dragaoCristalino,dragaoCristalino,dragaoCristalino,dragaoCristalino] 5 0 40
            onda3 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,
                          leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino] 4 0 70
            onda5 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,
                          leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino] 6 0 170
            onda6 = Onda [fungalith] 10 0 230
            onda8 = Onda [dragaoCristalino,dragaoCristalino,fungalith,dragaoCristalino,dragaoCristalino,fungalith] 5 0 320
            onda9 = Onda [aethralith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,
                          leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith] 5 0 380

portalIlhasCristalinas3 :: Portal
portalIlhasCristalinas3 = Portal (13.5,2.5) [onda4,onda5,onda6,onda7,onda9]
      where onda4 = Onda [fungalith] 4 0 140
            onda5 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera] 6 0 170
            onda6 = Onda [dragaoCristalino,dragaoCristalino] 10 0 230
            onda7 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,
                          leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino] 3 0 260
            onda9 = Onda [leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,
                          leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith] 5 0 385

portalIlhasCristalinas4 :: Portal
portalIlhasCristalinas4 = Portal (15.5,5.5) [onda4,onda5,onda6,onda7,onda9]
      where onda4 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino] 4 0 140
            onda5 = Onda [aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,
                          aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera,aranthisMortifera] 6 0 170
            onda6 = Onda [dragaoCristalino,dragaoCristalino] 10 0 230
            onda7 = Onda [leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,
                          leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino,leaoCristalino,leaoCristalino,leaoCristalino,dragaoCristalino] 3 0 260
            onda9 = Onda [leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,
                          leaoCristalino,leaoCristalino,dragaoCristalino,fungalith,leaoCristalino,leaoCristalino,dragaoCristalino,fungalith] 5 0 385

mapaIlhasCristalinas :: Mapa
mapaIlhasCristalinas = [[r,r,r,l,l,l,l,l,l,l,l,l,l,l,l,l,l,r,r,l,l],
                        [r,r,l,l,l,l,l,l,l,r,r,r,r,r,r,l,l,r,r,r,l],
                        [l,l,l,l,l,l,l,r,r,r,r,t,t,p,r,r,l,l,r,r,l],
                        [r,r,l,l,l,l,r,r,r,r,r,t,r,r,r,r,r,l,l,l,l],
                        [r,r,r,r,l,r,r,r,t,t,t,t,t,t,t,r,r,l,l,l,l],
                        [r,p,t,t,t,t,t,t,t,r,r,t,r,r,t,p,r,l,l,l,l],
                        [r,r,r,r,l,r,r,t,r,r,r,t,t,r,r,r,r,l,l,l,r],
                        [r,r,l,l,l,l,r,t,t,r,r,r,b,r,r,r,l,l,l,r,r],
                        [r,l,l,l,l,l,r,r,t,t,t,r,r,r,r,r,l,l,l,r,r],
                        [l,l,l,l,l,l,l,r,r,r,t,p,r,r,r,r,l,l,r,r,r],
                        [l,r,r,r,l,l,l,l,l,r,r,r,r,r,l,l,l,l,r,r,r],
                        [r,r,r,r,l,l,l,l,l,l,l,l,l,l,l,l,l,l,l,r,r],
                        [r,r,l,l,l,l,l,l,l,r,r,r,r,l,l,l,l,l,l,l,l],
                        [l,l,l,l,l,l,l,r,r,r,r,r,r,r,r,r,l,l,l,l,l]]
                              where p = Terra
                                    b = Terra
                                    t = Terra
                                    r = Relva
                                    l = LagrimasCristalinas

baseIlhasCristalinas :: Base
baseIlhasCristalinas = Base 100 (12.5,7.5) 1200

-- | Um dragão de cristal.

dragaoCristalino :: Inimigo
dragaoCristalino = Inimigo (0,0) Norte 350 0.7 30 30 [] [] 350 13

-- | Um leão de cristal.

leaoCristalino :: Inimigo
leaoCristalino = Inimigo (0,0) Norte 200 0.8 30 30 [] [Resina] 200 14

-- | Um monstro de fungos.

fungalith :: Inimigo
fungalith = Inimigo (0,0) Norte 2500 0.4 100 200 [] [] 2500 15

-- | Um Boss - deusa de cristal.

aethralith :: Inimigo
aethralith = Inimigo (0,0) Norte 8000 0.4 100 600 [] [Gelo] 8000 16





-- | As conquistas do jogo na forma de String.

conquista1 :: String
conquista1 = "Derrote 10 inimigos"

conquista2 :: String
conquista2 = "Derrote 100 inimigos"

conquista3 :: String
conquista3 = "Derrote 1000 inimigos"

conquista4 :: String
conquista4 = "Ganhe um nível sem perder nenhuma vida"

conquista5 :: String
conquista5 = "Ganhe o 'Vulcão das Trevas' sem perder nenhuma vida"

conquista6 :: String
conquista6 = "Ganhe todos os níveis principais"

conquista7 :: String
conquista7 = "Ganhe um nível com pelo menos 20 torres"

conquista8 :: String
conquista8 = "Ganhe um nível com pelo menos 6 tipos diferentes de torres"