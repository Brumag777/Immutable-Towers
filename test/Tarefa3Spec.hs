module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import LI12425
import Tarefa3

testesTarefa3 :: Test
testesTarefa3 = TestList [testTatualizaEfeitosChuva,testTregeneraInimigoChuvaLagrimasCristalinas,testTfiltraPosicoesLagrimasCristalinas,testTverifGelo,testTverifFogo,
                          testTverifResina,testTinimigoNaoEstaNoAlcance,testTdetetaInimigos,testTremoveInimigosMortos,testTinimigosAtingemBase,testTatualizaPosicao,
                          testTdivFloat,testTatualizaDirecao,testTatualizaInimigo,testTatualizaProjetil,testTatualizaPortal,testTmudaPosicaoInimigo,
                          testTatualizaOndaAtiva,testTatualizaTemposOnda,testTatualizaVidaInimigosLagrimasCristalinas,testTatualizaVidaInimigoLagrimasCristalinas,
                          testTremoveGotas,testTatualizaPosicoesGotas,testTremoveProjetilFogo,testTremoveProjetilGelo,testTremoveProjetilResina,
                          testTadicionaProjetilFogo,
                          testTatualizaTorres,testTatualizaInimigos,testTatualizaPortais]





-- {- Testes relativas às funções auxiliares -} --

-- | 'atualizaEfeitosChuva'

testTatualizaEfeitosChuva :: Test
testTatualizaEfeitosChuva = TestList 
  [atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 1 False 5 ~=?
    [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1],
   atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 1 True 5 ~=?
    [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1],
   atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 2 True 5 ~=?
    [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1],
   atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [] [Fogo] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Gelo Infinita] [] 1 1,
                         Inimigo (4,4) Norte 40 40 40 40 [Projetil Resina Infinita] [] 1 1] 3 True 5 ~=?
    [Inimigo (4,4) Norte 40 40 40 40 [] [Fogo] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo (Finita 5)] [] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo (Finita 5)] [] 1 1],
   atualizaEfeitosChuva [Inimigo (4,4) Norte 30 40 40 40 [] [] 100 1,Inimigo (4,4) Norte 90 40 40 40 [] [] 100 1] 4 True 5 ~=?
    [Inimigo (4,4) Norte 80 40 40 40 [] [] 100 1,Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1]]

-- | 'regeneraInimigoChuvaLagrimasCristalinas'

testTregeneraInimigoChuvaLagrimasCristalinas :: Test
testTregeneraInimigoChuvaLagrimasCristalinas = TestList 
  [regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 30 40 40 40 [] [] 100 1) ~=? Inimigo (4,4) Norte 80 40 40 40 [] [] 100 1,
   regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 90 40 40 40 [] [] 100 1) ~=? Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1,
   regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1) ~=? Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1]

-- | 'filtraPosicoesLagrimasCristalinas'

testTfiltraPosicoesLagrimasCristalinas :: Test
testTfiltraPosicoesLagrimasCristalinas = TestList [filtraPosicoesLagrimasCristalinas mapaX (0,0) ~=? [],
                                                   filtraPosicoesLagrimasCristalinas mapaY (0,0) ~=? [(2,0),(1,1),(2,1)]]

-- | 'verifGelo'

testTverifGelo :: Test
testTverifGelo = TestList [verifGelo (Projetil Gelo Infinita) ~=? True,
                           verifGelo (Projetil Fogo Infinita) ~=? False]

-- | 'verifFogo'

testTverifFogo :: Test
testTverifFogo = TestList [verifFogo (Projetil Fogo Infinita) ~=? True,
                           verifFogo (Projetil Resina Infinita) ~=? False]

-- | 'verifResina'

testTverifResina :: Test
testTverifResina = TestList [verifResina (Projetil Resina Infinita) ~=? True,
                             verifResina (Projetil Fraqueza Infinita) ~=? False]

-- | 'inimigoNaoEstaNoAlcance'

testTinimigoNaoEstaNoAlcance :: Test
testTinimigoNaoEstaNoAlcance = TestList [inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (5,4) Norte 50 50 50 50 [] [] 50 1) ~=? False,
                                         inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1) ~=? False,
                                         inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (9,4) Norte 50 50 50 50 [] [] 50 1) ~=? True,
                                         inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (8,8) Norte 50 50 50 50 [] [] 50 1) ~=? True]

-- | 'detetaInimigos'

testTdetetaInimigos :: Test
testTdetetaInimigos = TestList 
  [detetaInimigos (Torre (4,4) 5 4 2 5 (-5) (Projetil Fogo Infinita) 1 1 1 1) [Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,
                                                                             Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1] 5 1 1 ~=?
    (Torre (4,4) 5 4 2 5 5 (Projetil Fogo Infinita) 1 1 1 1,[Inimigo (5,5) Norte 45 50 50 50 [Projetil Fogo Infinita] [] 50 1,Inimigo (5,5) Norte 45 50 50 50 [Projetil Fogo Infinita] [] 50 1,
                                                           Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1])]

-- | 'removeInimigosMortos'

testTremoveInimigosMortos :: Test
testTremoveInimigosMortos = TestList 
  [removeInimigosMortos [Inimigo (5,5) Norte (-10) 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 0 50 50 50 [] [] 50 1,
                         Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1] (Base 50 (4,4) 50) 10 ~=?
    (Base 50 (4,4) 150,[Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1],12)]

-- | 'inimigosAtingemBase'

testTinimigosAtingemBase :: Test
testTinimigosAtingemBase = TestList 
  [inimigosAtingemBase (Base 200 (4,4) 100) [Inimigo (4.25,4) Norte 50 50 50 50 [] [] 50 1,Inimigo (4.25,4) Norte 50 50 50 50 [] [] 50 1,
                                             Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1] ~=?
    (Base 100 (4,4) 100,[Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1])]

-- | 'atualizaPosicao'

testTatualizaPosicao :: Test
testTatualizaPosicao = TestList [atualizaPosicao Norte (4,4) 3 1 ~=? (4,1),
                                 atualizaPosicao Sul (4,4) 3 1 ~=? (4,7),
                                 atualizaPosicao Este (4,4) 3 1 ~=? (7,4),
                                 atualizaPosicao Oeste (4,4) 3 1 ~=? (1,4)]

-- | 'divFloat'

testTdivFloat :: Test
testTdivFloat = divFloat 4.5 ~=? 0.5

-- | 'atualizaDirecao'

testTatualizaDirecao :: Test
testTatualizaDirecao = TestList 
  [atualizaDirecao Norte (0.5,0.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),
                                          (2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)] ~=? Este,
   atualizaDirecao Este (1.5,0.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),
                                         (2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)] ~=? Sul,
   atualizaDirecao Sul (1.5,4.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),
                                        (2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)] ~=? Este]

-- | 'atualizaInimigo'

testTatualizaInimigo :: Test
testTatualizaInimigo = TestList 
  [atualizaInimigo 1 1 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Fogo (Finita 3)] [] 100 1) ~=?
    Inimigo (1.5,0.5) Este 90 1 10 10 [Projetil Fogo (Finita 2)] [] 100 1,
   atualizaInimigo 1 1 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Gelo (Finita 3)] [] 100 1) ~=?
    Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Gelo (Finita 2)] [] 100 1,
   atualizaInimigo 1 1 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Resina Infinita] [] 100 1) ~=?
    Inimigo (1.2,0.5) Este 100 1 10 10 [Projetil Resina Infinita] [] 100 1]

-- | 'atualizaProjetil'

testTatualizaProjetil :: Test
testTatualizaProjetil = TestList [atualizaProjetil 1 [Projetil Fogo Infinita,Projetil Gelo (Finita 0.5)] ~=? [Projetil Fogo Infinita],
                                  atualizaProjetil 1 [Projetil Fogo Infinita,Projetil Gelo (Finita 1.5)] ~=? [Projetil Fogo Infinita,Projetil Gelo (Finita 0.5)]]

-- | 'atualizaPortal'

testTatualizaPortal :: Test
testTatualizaPortal = TestList 
  [atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5]) 1 [] ~=?
    (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-6) 4],[]),
   atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) 1 [] ~=?
    atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) 1 []]

-- | 'mudaPosicaoInimigo'

testTmudaPosicaoInimigo :: Test
testTmudaPosicaoInimigo = mudaPosicaoInimigo (Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1) (4,4) ~=? Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1

-- | 'atualizaOndaAtiva'

testTatualizaOndaAtiva :: Test
testTatualizaOndaAtiva = TestList 
  [atualizaOndaAtiva (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 5 (-5)) 1 (4,4) ~=?
    (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 4 (-5),[]),
   atualizaOndaAtiva (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-4) (-5)) 1 (4,4) ~=?
    (Onda [] 5 5 (-5),[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1])]

-- | 'atualizaTemposOnda'

testTatualizaTemposOnda :: Test
testTatualizaTemposOnda = atualizaTemposOnda 1 (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 5 100) ~=? Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 4 99

-- | 'atualizaVidaInimigosLagrimasCristalinas'

testTatualizaVidaInimigosLagrimasCristalinas :: Test
testTatualizaVidaInimigosLagrimasCristalinas = TestList 
  [atualizaVidaInimigosLagrimasCristalinas [(1,1)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (True,30,[]) ~=?
    [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1],
   atualizaVidaInimigosLagrimasCristalinas [(1,1)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (False,30,[]) ~=?
    [Inimigo (1,1) Norte 45 40 40 40 [] [] 45 1],
   atualizaVidaInimigosLagrimasCristalinas [(5,5)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (True,30,[]) ~=?
    [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1]]

-- | 'atualizaVidaInimigoLagrimasCristalinas'

testTatualizaVidaInimigoLagrimasCristalinas :: Test
testTatualizaVidaInimigoLagrimasCristalinas = TestList 
  [atualizaVidaInimigoLagrimasCristalinas [(1,1)] 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1) ~=?
    Inimigo (1,1) Norte 45 40 40 40 [] [] 45 1,
   atualizaVidaInimigoLagrimasCristalinas [(5,5)] 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1) ~=?
    Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1]

-- | 'removeGotas'

testTremoveGotas :: Test
testTremoveGotas = removeGotas [(0,-900),(0,-200)] ~=? [(0,-200)]

-- | 'atualizaPosicoesGotas'

testTatualizaPosicoesGotas :: Test
testTatualizaPosicoesGotas = atualizaPosicoesGotas [(100,-300),(200,-400)] 1 ~=? [(95,-400),(195,-500)]

-- | 'removeProjetilFogo'

testTremoveProjetilFogo :: Test
testTremoveProjetilFogo = removeProjetilFogo (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1) ~=?
  Inimigo (1,1) Norte 40 40 40 40 [Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1

-- | 'removeProjetilGelo'

testTremoveProjetilGelo :: Test
testTremoveProjetilGelo = removeProjetilGelo (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1) ~=?
  Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1

-- | 'adicionaProjetilFogo'

testTadicionaProjetilFogo :: Test
testTadicionaProjetilFogo = TestList 
  [adicionaProjetilFogo 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1) ~=? Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo (Finita 1)] [] 45 1,
   adicionaProjetilFogo 1 (Inimigo (1,1) Norte 40 40 40 40 [] [Fogo] 45 1) ~=? Inimigo (1,1) Norte 40 40 40 40 [] [Fogo] 45 1]

-- | 'removeProjetilResina'

testTremoveProjetilResina :: Test
testTremoveProjetilResina = removeProjetilResina (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1) ~=?
  Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Fraqueza Infinita] [] 45 1





-- {- Testes relativos às funções principais -} --

-- | 'atualizaTorres'

testTatualizaTorres :: Test
testTatualizaTorres = atualizaTorres [Torre (1,1) 5 1 1 5 0 (Projetil Fogo Infinita) 1 1 1 1,Torre (4,4) 5 1 1 5 1 (Projetil Fogo Infinita) 1 1 1 1] 
                                     [Inimigo (1,1) Norte 40 40 40 40 [] [] 40 1,Inimigo (4,4) Norte 40 40 40 40 [] [] 40 1] 1 1 1 ~=?
                                     ([Torre (1,1) 5 1 1 5 5 (Projetil Fogo Infinita) 1 1 1 1,Torre (4,4) 5 1 1 5 0 (Projetil Fogo Infinita) 1 1 1 1],
                                      [Inimigo (4,4) Norte 40 40 40 40 [] [] 40 1,Inimigo (1,1) Norte 35 40 40 40 [Projetil Fogo Infinita] [] 40 1])

-- | 'atualizaInimigos'

testTatualizaInimigos :: Test
testTatualizaInimigos = atualizaInimigos [Inimigo (0.5,0.5) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1,Inimigo (0.5,0.5) Norte 40 1 40 40 [] [] 40 1] 
                                          mapa01 (Base 100 (1,4) 100) 1 5 1 False ~=?
                                          (Base 100 (1,4) 100,[Inimigo (1.5,0.5) Este (-5) 1 40 40 [Projetil Fogo Infinita] [] 40 1,Inimigo (1.5,0.5) Este 40 1 40 40 [] [] 40 1],1)

-- | 'atualizaPortais'

testTatualizaPortais :: Test
testTatualizaPortais = atualizaPortais [Portal (4,4) [Onda [Inimigo (0.5,0.5) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1] 5 0 (-5)]] 1 [] ~=?
                                        ([Portal (4,4) [Onda [] 5 5 (-5)]],[Inimigo (4,4) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1])