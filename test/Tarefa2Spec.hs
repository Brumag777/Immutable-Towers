module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425

testesTarefa2 :: Test
testesTarefa2 = TestList [testTinimigoEstaNoAlcance,testTsomaDuracoes,testTverifFraqueza,testTverifNaoFraqueza,testTnormalizaProjeteis,testTnormalizaProjeteisNaoFraqueza,
                          testTnovosProjeteisInimigo,testTexistOndaAtiva,testTposicaoOndaAtiva,
                          testTinimigosNoAlcance,testTatingeInimigo,testTativaInimigo,testTterminouJogo,testTganhouJogo,testTperdeuJogo]





-- {- Testes relativas às funções auxiliares -} --

-- | 'inimigoEstaNoAlcance'

testTinimigoEstaNoAlcance :: Test
testTinimigoEstaNoAlcance = TestList [inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (5,4) Norte 50 50 50 50 [] [] 50 1) ~=? True,
                                      inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1) ~=? True,
                                      inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (9,4) Norte 50 50 50 50 [] [] 50 1) ~=? False,
                                      inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1 1 1) (Inimigo (8,8) Norte 50 50 50 50 [] [] 50 1) ~=? False]

-- | 'somaDuracoes'

testTsomaDuracoes :: Test
testTsomaDuracoes = TestList [somaDuracoes (Projetil Fogo (Finita 4)) (Projetil Fogo (Finita 5)) ~=? Projetil Fogo (Finita 9),
                              somaDuracoes (Projetil Gelo Infinita) (Projetil Gelo (Finita 5)) ~=? Projetil Gelo Infinita,
                              somaDuracoes (Projetil Resina (Finita 5)) (Projetil Resina Infinita) ~=? Projetil Resina Infinita,
                              somaDuracoes (Projetil Fraqueza Infinita) (Projetil Fraqueza Infinita) ~=? Projetil Fraqueza Infinita]

-- | 'verifFraqueza'

testTverifFraqueza :: Test
testTverifFraqueza = TestList [verifFraqueza (Projetil Fraqueza Infinita) ~=? True,
                               verifFraqueza (Projetil Fraqueza (Finita 3)) ~=? True,
                               verifFraqueza (Projetil Fogo Infinita) ~=? False]

-- | 'verifNaoFraqueza'

testTverifNaoFraqueza :: Test
testTverifNaoFraqueza = TestList [verifNaoFraqueza (Projetil Fraqueza Infinita) ~=? False,
                                  verifNaoFraqueza (Projetil Fraqueza (Finita 3)) ~=? False,
                                  verifNaoFraqueza (Projetil Fogo Infinita) ~=? True]

-- | 'normalizaProjeteis'

testTnormalizaProjeteis :: Test
testTnormalizaProjeteis = TestList
  [normalizaProjeteis [Projetil Fraqueza (Finita 5),Projetil Fraqueza Infinita,Projetil Gelo Infinita,Projetil Fogo Infinita] ~=? [Projetil Fraqueza Infinita],
   normalizaProjeteis [Projetil Fraqueza (Finita 5),Projetil Fraqueza (Finita 1),Projetil Resina Infinita,Projetil Fogo (Finita 4)] ~=? [Projetil Fraqueza (Finita 6),Projetil Fogo (Finita 8)]]

-- | 'normalizaProjeteisNaoFraqueza'

testTnormalizaProjeteisNaoFraqueza :: Test
testTnormalizaProjeteisNaoFraqueza = TestList [normalizaProjeteisNaoFraqueza [Projetil Gelo Infinita,Projetil Fogo Infinita] ~=? [],
                                               normalizaProjeteisNaoFraqueza [Projetil Resina (Finita 1),Projetil Fogo (Finita 3)] ~=? [Projetil Fogo (Finita 6)]]

-- | 'novosProjeteisInimigo'

testTnovosProjeteisInimigo :: Test
testTnovosProjeteisInimigo = TestList 
  [novosProjeteisInimigo (Projetil Fogo Infinita) [Projetil Gelo Infinita] ~=? [],
   novosProjeteisInimigo (Projetil Fraqueza (Finita 4)) [Projetil Resina Infinita,Projetil Fraqueza (Finita 6)] ~=? [Projetil Fraqueza (Finita 10),Projetil Resina Infinita]]

-- | 'existOndaAtiva'

testTexistOndaAtiva :: Test
testTexistOndaAtiva = TestList [existOndaAtiva [] ~=? False,
                                existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5] ~=? False,
                                existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)] ~=? True,
                                existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)] ~=? True,
                                existOndaAtiva [Onda [] 5 (-5) (-5)] ~=? False]

-- | 'posicaoOndaAtiva'

testTposicaoOndaAtiva :: Test
testTposicaoOndaAtiva = TestList [posicaoOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)] ~=? 0,
                                  posicaoOndaAtiva [Onda [] 40 (-5) (-5),Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)] ~=? 1]





-- {- Testes relativos às funções principais -} --

-- | 'inimigosNoAlcance'

testTinimigosNoAlcance :: Test
testTinimigosNoAlcance = TestList 
  [inimigosNoAlcance (Torre (5,5) 5 2 5 5 5 (Projetil Fraqueza Infinita) 1 1 1 1) [Inimigo (2,3) Norte 40 40 40 40 [] [] 1 1,Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] ~=?
   [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1],
   inimigosNoAlcance (Torre (5,5) 5 2 5 5 5 (Projetil Fraqueza Infinita) 1 1 1 1) [] ~=? []]

-- | 'atingeInimigo'

testTatingeInimigo :: Test
testTatingeInimigo = TestList 
  [atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1 1 1) 1 1 (Inimigo (2,3) Norte 40 40 40 40 [Projetil Gelo Infinita] [Fogo] 1 1) ~=?
    Inimigo (2,3) Norte 35 40 40 40 [Projetil Gelo Infinita] [Fogo] 1 1,
   atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1 1 1) 1 1 (Inimigo (2,3) Norte 40 40 40 40 [Projetil Gelo Infinita] [] 1 1) ~=?
    Inimigo (2,3) Norte 35 40 40 40 [] [] 1 1,
   atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1 1 1) 1 1 (Inimigo (2,3) Norte 40 40 40 40 [Projetil Fraqueza Infinita] [] 1 1) ~=?
    Inimigo (2,3) Norte 30 40 40 40 [Projetil Fraqueza Infinita,Projetil Fogo (Finita 4)] [] 1 1]

-- | 'ativaInimigo'

testTativaInimigo :: Test
testTativaInimigo = TestList 
  [ativaInimigo (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) [] ~=?
    (Portal (4,4) [Onda [] 5 5 (-5)],[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1]),
   ativaInimigo (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) [] ~=?
    (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [] 5 5 (-5)],[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1])]

-- | 'terminouJogo'

testTterminouJogo :: Test
testTterminouJogo = TestList [terminouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? True,
                              terminouJogo (Jogo (Base (-10) (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? True,
                              terminouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? False]

-- | 'ganhouJogo'

testTganhouJogo :: Test
testTganhouJogo = TestList [ganhouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? True,
                            ganhouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? False]

-- | 'perdeuJogo'

testTperdeuJogo :: Test
testTperdeuJogo = TestList [perdeuJogo (Jogo (Base 50 (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? False,
                            perdeuJogo (Jogo (Base (-10) (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]) 1) ~=? True]