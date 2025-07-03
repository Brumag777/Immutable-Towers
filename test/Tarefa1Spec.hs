{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import LI12425
import Tarefa1

testesTarefa1 :: Test
testesTarefa1 = TestList [testTdist,testTencontraPontosAdjacentes,testTfiltraPosicoesTerra,testTfiltraPosicoesRelva,testTremoveElemento,testTremoveElementosRepetidos,
                          testTverifExistPortais,testTverifPortaisSobreTerra,testTverifConexaoBasePortais,testTposicoesPossiveisParaPortais,testTverifSobreposPortaisTorresBase,
                          testTverifOndasAtivasPorPortal,
                          testTverifInimigosInativos,testTverifPosicaoInimigoPortal,testTvidaInimigoPositiva,testTlistaProjeteisAtivosInimigos,testTtodosInimigosSobreTerra,
                          testTinimigosSobreposTorres,testTvelocidadeInimigosNaoNegativa,testTverifProjeteisNormalizados,
                          testTverifTorresSobreRelva,testTverifAlcanceTorres,testTverifRajadaTorres,testTverifCicloTorres,testTverifSobreposTorres,
                          testTverifBaseSobreTerra,testTverifCreditosBase,testTverifSobreposBaseTorresPortais,
                          testTvalidaJogo]





-- {- Testes relativas às funções auxiliares -} --

-- | 'dist'

testTdist :: Test
testTdist = TestList [dist (0,0) (0,1) ~=? 1,
                      dist (1,1) (2,2) ~=? sqrt 2,
                      dist (-1,2) (-4,-2) ~=? 5]

-- | 'encontraPontosAdjacentes'

testTencontraPontosAdjacentes :: Test
testTencontraPontosAdjacentes = TestList [encontraPontosAdjacentes (4,4) [(5,5),(3,5),(3,3)] ~=? [],
                                          encontraPontosAdjacentes (2,3) [(3,3),(3,4),(3,5)] ~=? [(3,3)],
                                          encontraPontosAdjacentes (2,2) [(2,3),(2,1),(1,2)] ~=? [(2,3),(2,1),(1,2)]]

-- | 'filtraPosicoesTerra'

testTfiltraPosicoesTerra :: Test
testTfiltraPosicoesTerra = filtraPosicoesTerra mapa01 (0,0) ~=? [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),
                                                                 (2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0)]

-- | 'filtraPosicoesRelva'

testTfiltraPosicoesRelva :: Test
testTfiltraPosicoesRelva = filtraPosicoesRelva mapa01 (0,0) ~=? [(2.0,0.0),(0.0,1.0),(2.0,1.0),(5.0,1.0),(0.0,2.0),(2.0,2.0),(0.0,3.0),(4.0,3.0),(0.0,4.0),(4.0,5.0),(5.0,5.0)]

-- | 'removeElemento'

testTremoveElemento :: Test
testTremoveElemento = TestList [removeElemento 3 [1,2,3,4,5] ~=? [1,2,4,5],
                                removeElemento 6 [1,2,3,4,5] ~=? [1,2,3,4,5]]

-- | 'removeElementosRepetidos'

testTremoveElementosRepetidos :: Test
testTremoveElementosRepetidos = TestList [removeElementosRepetidos [1,2,3,4,4,5,5] ~=? [1,2,3,4,5],
                                          removeElementosRepetidos [1,2,3,4,5] ~=? [1,2,3,4,5]]





-- {- Testes relativos às funções dos portais -} --

-- | 'verifExistPortais'

testTverifExistPortais :: Test
testTverifExistPortais = TestList [verifExistPortais [] ~=? False,
                                   verifExistPortais [Portal (2,2) []] ~=? True]

-- | 'verifPortaisSobreTerra'

testTverifPortaisSobreTerra :: Test
testTverifPortaisSobreTerra = TestList [verifPortaisSobreTerra [(1,0)] mapa01 ~=? True,
                                        verifPortaisSobreTerra [(2,0)] mapa01 ~=? False,
                                        verifPortaisSobreTerra [(1,0),(2,0)] mapa01 ~=? False]

-- | 'verifConexaoBasePortais'

testTverifConexaoBasePortais :: Test
testTverifConexaoBasePortais = TestList [verifConexaoBasePortais (0,0) [(2,3)] (filtraPosicoesTerra mapa01 (0,0)) ~=? True,
                                         verifConexaoBasePortais (0,0) [(4,0)] (filtraPosicoesTerra mapa01 (0,0)) ~=? False,
                                         verifConexaoBasePortais (0,0) [(2,3),(4,0)] (filtraPosicoesTerra mapa01 (0,0)) ~=? True]

-- | 'posicoesPossiveisParaPortais'

testTposicoesPossiveisParaPortais :: Test
testTposicoesPossiveisParaPortais = TestList [posicoesPossiveisParaPortais (filtraPosicoesTerra mapa01 (0,0)) (0,0) ~=? [(0.0,0.0),(1.0,0.0),(1.0,1.0),(1.0,2.0),(1.0,3.0),
                                              (2.0,3.0),(2.0,4.0),(1.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(5.0,3.0),(5.0,2.0),(1.0,4.0),(2.0,4.0),(2.0,3.0),(3.0,4.0),(4.0,4.0),
                                              (5.0,4.0),(5.0,3.0),(5.0,2.0)],
                                              posicoesPossiveisParaPortais (filtraPosicoesTerra mapa01 (0,0)) (4,0) ~=? [(4.0,0.0),(4.0,1.0)]]

-- | 'verifSobreposPortaisTorresBase'

testTverifSobreposPortaisTorresBase :: Test
testTverifSobreposPortaisTorresBase = TestList [verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2)] (3,4) ~=? True,
                                                verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2)] (3,3) ~=? False,
                                                verifSobreposPortaisTorresBase [(0,0)] [(1,1),(2,2)] (3,3) ~=? True,
                                                verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2),(0,0)] (3,4) ~=? False,
                                                verifSobreposPortaisTorresBase [(3,3)] [(1,1),(2,2)] (3,4) ~=? True,
                                                verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(3,3)] (0,0) ~=? False]

-- | 'verifOndasAtivasPorPortal'

testTverifOndasAtivasPorPortal :: Test
testTverifOndasAtivasPorPortal = TestList [verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 5,
                                           Onda [Inimigo (1,1) Este 10 10 10 10 [] [] 10 1] 10 10 10]) ~=? True,
                                           verifOndasAtivasPorPortal (Portal (0,0) [Onda [] 5 5 (-5),
                                           Onda [] 10 10 (-10)]) ~=? True,
                                           verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 (-5),
                                           Onda [] 10 10 (-10)]) ~=? True,
                                           verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 (-5),
                                           Onda [Inimigo (1,1) Este 10 10 10 10 [] [] 10 1] 10 10 (-10)]) ~=? False]





-- {- Testes relativos às funções dos inimigos -} --

-- | 'verifInimigosInativos'

testTverifInimigosInativos :: Test
testTverifInimigosInativos = TestList [verifInimigosInativos (Portal (2,2) []) ~=? True,
                                       verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 1 1 1 1 [] [] 1 1] 1 1 1]) ~=? True,
                                       verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,3) Norte 1 1 1 1 [] [] 1 1] 1 1 1]) ~=? False,
                                       verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 0 1 1 1 [] [] 1 1] 1 1 1]) ~=? False,
                                       verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1] 1 1 1]) ~=? False]

-- | 'verifPosicaoInimigoPortal'

testTverifPosicaoInimigoPortal :: Test
testTverifPosicaoInimigoPortal = TestList [verifPosicaoInimigoPortal [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]] (1,1) ~=? True,
                                           verifPosicaoInimigoPortal [[Inimigo (1,2) Norte 1 1 1 1 [] [] 1 1]] (1,1) ~=? False,
                                           verifPosicaoInimigoPortal [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1],[Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]] (1,1) ~=? False]

-- | 'vidaInimigoPositiva'

testTvidaInimigoPositiva :: Test
testTvidaInimigoPositiva = TestList [vidaInimigoPositiva [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]] ~=? True,
                                     vidaInimigoPositiva [[Inimigo (1,1) Norte (-1) 1 1 1 [] [] 1 1]] ~=? False,
                                     vidaInimigoPositiva [[Inimigo (1,1) Norte (-1) 1 1 1 [] [] 1 1],[Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]] ~=? False]

-- | 'listaProjeteisAtivosInimigos'

testTlistaProjeteisAtivosInimigos :: Test
testTlistaProjeteisAtivosInimigos = TestList [listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]] ~=? True,
                                              listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1]] ~=? False,
                                              listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1],
                                                                            [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]] ~=? False]

-- | 'todosInimigosSobreTerra'

testTtodosInimigosSobreTerra :: Test
testTtodosInimigosSobreTerra = TestList [todosInimigosSobreTerra [(1,1),(2,3)] (filtraPosicoesTerra mapa01 (0,0)) ~=? True,
                                         todosInimigosSobreTerra [(1,1),(2,3),(2,0)] (filtraPosicoesTerra mapa01 (0,0)) ~=? False]

-- | 'inimigosSobreposTorres'

testTinimigosSobreposTorres :: Test
testTinimigosSobreposTorres = TestList [inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] 
                                        [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1] ~=? True,
                                        inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] 
                                        [Torre (2,2) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1] ~=? False,
                                        inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,0) Norte 2 2 2 2 [] [] 2 1] 
                                        [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1] ~=? False]

-- | 'velocidadeInimigosNaoNegativa'

testTvelocidadeInimigosNaoNegativa :: Test
testTvelocidadeInimigosNaoNegativa = TestList [velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] ~=? True,
                                               velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 0 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] ~=? True,
                                               velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 (-1) 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] ~=? False]

-- | 'verifProjeteisNormalizados'

testTverifProjeteisNormalizados :: Test
testTverifProjeteisNormalizados = TestList [verifProjeteisNormalizados [Projetil Fogo Infinita] ~=? True,
                                            verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Fogo (Finita 1)] ~=? False,
                                            verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Gelo (Finita 1)] ~=? False,
                                            verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Resina Infinita] ~=? False,
                                            verifProjeteisNormalizados [Projetil Gelo Infinita,Projetil Resina Infinita] ~=? True,
                                            verifProjeteisNormalizados [Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] ~=? True]





-- {- Testes relativos às funções das torres -} --

-- | 'verifTorresSobreRelva'

testTverifTorresSobreRelva :: Test
testTverifTorresSobreRelva = TestList [verifTorresSobreRelva [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1] mapa01 ~=? True,
                                       verifTorresSobreRelva [Torre (1,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1] mapa01 ~=? False,
                                       verifTorresSobreRelva [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1 1 1,Torre (1,0) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1 1 1] mapa01 ~=? False]

-- | 'verifAlcanceTorres'

testTverifAlcanceTorres :: Test
testTverifAlcanceTorres = TestList [verifAlcanceTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1 1 1] ~=? True,
                                    verifAlcanceTorres [Torre (1,1) 1 0 1 1 1 (Projetil Resina Infinita) 1 1 1 1] ~=? True,
                                    verifAlcanceTorres [Torre (1,1) 1 (-1) 1 1 1 (Projetil Resina Infinita) 1 1 1 1] ~=? False]

-- | 'verifRajadaTorres'

testTverifRajadaTorres :: Test
testTverifRajadaTorres = TestList [verifRajadaTorres [Torre (1,1) 1 1 1 1 1 (Projetil Fraqueza Infinita) 1 1 1 1] ~=? True,
                                   verifRajadaTorres [Torre (1,1) 1 1 0 1 1 (Projetil Fraqueza Infinita) 1 1 1 1] ~=? False,
                                   verifRajadaTorres [Torre (1,1) 1 1 (-1) 1 1 (Projetil Fraqueza Infinita) 1 1 1 1] ~=? False]

-- | 'verifCicloTorres'

testTverifCicloTorres :: Test
testTverifCicloTorres = TestList [verifCicloTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1 1 1] ~=? True,
                                  verifCicloTorres [Torre (1,1) 1 1 1 0 1 (Projetil Resina Infinita) 1 1 1 1] ~=? True,
                                  verifCicloTorres [Torre (1,1) 1 1 1 (-1) 1 (Projetil Resina Infinita) 1 1 1 1] ~=? False]

-- | 'verifSobreposTorres'

testTverifSobreposTorres :: Test
testTverifSobreposTorres = TestList [verifSobreposTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1 1 1,Torre (2,2) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1 1 1] ~=? True,
                                     verifSobreposTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1 1 1,Torre (1,1) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1 1 1] ~=? False]





-- {- Testes relativos às funções da base -} --

-- | 'verifBaseSobreTerra'

testTverifBaseSobreTerra :: Test
testTverifBaseSobreTerra = TestList [verifBaseSobreTerra (Base 1 (0,0) 1) mapa01 ~=? True,
                                     verifBaseSobreTerra (Base 1 (2,2) 1) mapa01 ~=? False]

-- | 'verifCreditosBase'

testTverifCreditosBase :: Test
testTverifCreditosBase = TestList [verifCreditosBase (Base 1 (0,0) 1) ~=? True,
                                   verifCreditosBase (Base 1 (0,0) 0) ~=? True,
                                   verifCreditosBase (Base 1 (0,0) (-1)) ~=? False]

-- | 'verifSobreposBaseTorresPortais'

testTverifSobreposBaseTorresPortais :: Test
testTverifSobreposBaseTorresPortais = TestList [verifSobreposBaseTorresPortais (1,1) [(0,0),(2,2)] [(3,3),(4,4)] ~=? True,
                                                verifSobreposBaseTorresPortais (1,1) [(0,0),(1,1)] [(3,3),(4,4)] ~=? False,
                                                verifSobreposBaseTorresPortais (1,1) [(0,0),(2,2)] [(1,1),(4,4)] ~=? False,
                                                verifSobreposBaseTorresPortais (1,1) [(1,1),(2,2)] [(1,1),(4,4)] ~=? False]





-- {- Teste relativo à função principal -} --

-- | 'validaJogo'

testTvalidaJogo :: Test
testTvalidaJogo = TestList 
    [validaJogo (Jogo (Base 100 (0,0) 100) [Portal (1,0) [Onda [Inimigo (1,0) Norte 100 1 5 5 [] [] 100 1] 5 1 (-5)]] [] mapa01 [] [] 1 (0,0) 0 (False,5,[]) 1) ~=? True,
     validaJogo (Jogo (Base 100 (0,0) 100) [Portal (3,0) [Onda [Inimigo (1,0) Norte 100 1 5 5 [] [] 100 1] 5 1 (-5)]] [] mapa01 [] [] 1 (0,0) 0 (False,5,[]) 1) ~=? False]