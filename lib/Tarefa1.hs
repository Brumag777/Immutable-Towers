{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use any" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Bruno Miguel Silva Magalhães <a110274@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}

module Tarefa1 where

import LI12425





-- | A função 'validaJogo' verifica se um dado jogo é ou não válido baseando-se numa série de condições verificadas por outras funções.
--
-- == Exemplos
--
-- >>> validaJogo (Jogo (Base 100 (0,0) 100) [Portal (1,0) [Onda [Inimigo (1,0) Norte 100 1 5 5 [] [] 100 1] 5 1 (-5)]] [] mapa01 [] [] 1 (0,0) 0 (False,5,[]))
-- True
--
-- >>> validaJogo (Jogo (Base 100 (0,0) 100) [Portal (3,0) [Onda [Inimigo (1,0) Norte 100 1 5 5 [] [] 100 1] 5 1 (-5)]] [] mapa01 [] [] 1 (0,0) 0 (False,5,[]))
-- False

validaJogo :: 
 -- | Jogo a validar.
 Jogo ->
 -- | Resultado na forma 'True' caso o jogo é válido ou 'False' caso contrário.
 Bool
validaJogo jogo = verifExistPortais portais &&
                  verifPortaisSobreTerra posicoesPortais mapa &&
                  verifConexaoBasePortais posBase posicoesPortais (filtraPosicoesTerra mapa (0,0)) &&
                  verifSobreposPortaisTorresBase posicoesPortais posicoesTorres posBase &&
                  and (map verifOndasAtivasPorPortal portais) &&
                  and (map verifInimigosInativos portais) &&
                  todosInimigosSobreTerra (map posicaoInimigo inimigosAtivos) (filtraPosicoesTerra mapa (0,0)) &&
                  inimigosSobreposTorres inimigosAtivos torres &&
                  velocidadeInimigosNaoNegativa inimigosAtivos &&
                  and (map verifProjeteisNormalizados (map projeteisInimigo inimigosAtivos)) &&
                  verifTorresSobreRelva torres mapa &&
                  verifAlcanceTorres torres &&
                  verifRajadaTorres torres &&
                  verifCicloTorres torres &&
                  verifSobreposTorres torres &&
                  verifBaseSobreTerra base mapa &&
                  verifCreditosBase base &&
                  verifSobreposBaseTorresPortais posBase posicoesTorres posicoesPortais
                    where mapa = mapaJogo jogo
                          base = baseJogo jogo
                          posBase = posicaoBase base
                          portais = portaisJogo jogo
                          posicoesPortais = map posicaoPortal portais
                          torres = torresJogo jogo
                          posicoesTorres = map posicaoTorre torres
                          inimigosAtivos = inimigosJogo jogo

-- | 'mapa01' é um exemplo de um mapa.

mapa01 :: Mapa
mapa01 = [[t, t, r, a, t, a],
          [r, t, r, a, t, r],
          [r, t, r, a, a, t],
          [r, t, t, a, r, t],
          [r, t, t, t, t, t],
          [a, a, a, a, r, r]]
            where t = Terra
                  r = Relva
                  a = Agua





-- {- Funções auxiliares -} --

-- | A função 'dist' determina a distância entre dois pontos de um plano.
--
-- == Exemplos
--
-- >>> dist (0,0) (4,3)
-- 5
--
-- >>> dist (2,2) (2,3)
-- 1

dist :: 
 -- | Posição do primeiro ponto.
 Posicao -> 
 -- | Posição do segundo ponto.
 Posicao ->
 -- | Distância entre os dois pontos.
 Float
dist (x1,y1) (x2,y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

-- | A função 'encontraPontosAdjacentes' filtra, de uma lista de pontos, apenas aqueles que se encontram adjacentes (não diagonalmente) a um outro ponto de um plano.
--
-- == Exemplos
--
-- >>> encontraPontosAdjacentes (4,4) [(5,5),(3,5),(3,3)]
-- []
--
-- >>> encontraPontosAdjacentes (2,3) [(3,3),(3,4),(3,5)]
-- [(3,3)]
--
-- >>> encontraPontosAdjacentes (2,2) [(2,3),(2,1),(1,2)]
-- [(2,3),(2,1),(1,2)]

encontraPontosAdjacentes :: 
 -- | Ponto de foco.
 Posicao -> 
 -- | Lista de pontos.
 [Posicao] -> 
 -- | Pontos (não diagonalmente) adjacentes ao ponto de foco.
 [Posicao]
encontraPontosAdjacentes _ [] = []
encontraPontosAdjacentes p (h:t)
    | dist p h == 1 = h : encontraPontosAdjacentes p t
    | otherwise = encontraPontosAdjacentes p t

-- | A função 'filtraPosicoesTerra' determina as coordenadas de pontos correnpondentes apenas ao terreno de 'Terra' de um mapa.
--
-- == Exemplo
--
-- >>> filtraPosicoesTerra mapa01 (0,0)
-- [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),(2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0)]

filtraPosicoesTerra :: 
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição do ponto na matriz (deve ser (0,0) inicialmente para determinar corretamente as coordenadas).
 Posicao ->
 -- | Posições correpondentes ao terreno de 'Terra'.
 [Posicao]
filtraPosicoesTerra [] _ = []
filtraPosicoesTerra (l:ls) (x,y) = filtraPosicoesTerraAux l (0,y) ++ filtraPosicoesTerra ls (x,y + 1)
    where filtraPosicoesTerraAux [] _ = []
          filtraPosicoesTerraAux (c:cs) (a,b)
            | c == Terra = (a,b) : filtraPosicoesTerraAux cs (a + 1,b)
            | otherwise = filtraPosicoesTerraAux cs (a + 1,b)

-- | A função 'filtraPosicoesRelva' determina as coordenadas de pontos correnpondentes apenas ao terreno de 'Relva' de um mapa.
--
-- == Exemplo
--
-- >>> filtraPosicoesRelva mapa01 (0,0)
-- [(2.0,0.0),(0.0,1.0),(2.0,1.0),(5.0,1.0),(0.0,2.0),(2.0,2.0),(0.0,3.0),(4.0,3.0),(0.0,4.0),(4.0,5.0),(5.0,5.0)]

filtraPosicoesRelva :: 
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição do ponto na matriz (deve ser (0,0) inicialmente para determinar corretamente as coordenadas).
 Posicao ->
 -- | Posições correpondentes ao terreno de 'Relva'.
 [Posicao]
filtraPosicoesRelva [] _ = []
filtraPosicoesRelva (l:ls) (x,y) = filtraPosicoesRelvaAux l (0,y) ++ filtraPosicoesRelva ls (x,y + 1)
    where filtraPosicoesRelvaAux [] _ = []
          filtraPosicoesRelvaAux (c:cs) (a,b)
            | c == Relva = (a,b) : filtraPosicoesRelvaAux cs (a + 1,b)
            | otherwise = filtraPosicoesRelvaAux cs (a + 1,b)

-- | A função 'removeElemento' remove as ocorrências de um elemento específico de uma lista.
--
-- == Exemplos
--
-- >>> removeElemento 3 [1,2,3,4,5]
-- [1,2,4,5]
--
-- >>> removeElemento 6 [1,2,3,4,5]
-- [1,2,3,4,5]

removeElemento :: Eq a => 
 -- | Elemento a remover.
 a -> 
 -- | Lista inicial.
 [a] ->
 -- | Lista resultante.
 [a]
removeElemento e = filter (/= e)

-- | A função 'removeElementosRepetidos' remove todos os elementos repetidos numa lista.
--
-- == Exemplos
--
-- >>> removeElementosRepetidos [1,2,3,4,4,5,5]
-- [1,2,3,4,5]
--
-- >>> removeElementosRepetidos [1,2,3,4,5]
-- [1,2,3,4,5]

removeElementosRepetidos :: Eq a => 
 -- | Lista inicial.
 [a] ->
 -- | Lista resultante.
 [a]
removeElementosRepetidos [] = []
removeElementosRepetidos (h:t)
    | h `elem` t = removeElementosRepetidos t
    | otherwise = h : removeElementosRepetidos t





-- {- Funções relativas aos portais -} --

-- | __(1.a)__ A função 'verifExistPortais' verifica se existe pelo menos um portal durante o jogo.
--
-- == Exemplos
--
-- >>> verifExistPortais []
-- False
--
-- >>> verifExistPortais [Portal (2,2) []]
-- True

verifExistPortais :: 
 -- | Lista dos portais.
 [Portal] -> 
 -- | Resultado na forma 'True' caso existe pelo menos um portal ou 'False' caso contrário.
 Bool
verifExistPortais portais = not $ null portais

-- | __(1.b)__ A função 'verifPortaisSobreTerra' verifica se todos os portais do jogo estão posicionados sobre terra.
--
-- == Exemplos
--
-- >>> verifPortaisSobreTerra [(1,0)] mapa01
-- True
--
-- >>> verifPortaisSobreTerra [(2,0)] mapa01
-- False
--
-- >>> verifPortaisSobreTerra [(1,0),(2,0)] mapa01
-- False

verifPortaisSobreTerra :: 
 -- | Posições dos portais.
 [Posicao] ->
 -- | Mapa do jogo.
 Mapa ->
 -- | Resultado na forma 'True' se todos os portais estão no caminho do terra ou 'False' caso contrário.
 Bool
verifPortaisSobreTerra [] _ = True
verifPortaisSobreTerra (h:t) mapa = verifPortaisSobreTerraAux h mapa && verifPortaisSobreTerra t mapa
    where verifPortaisSobreTerraAux :: Posicao -> Mapa -> Bool
          verifPortaisSobreTerraAux (x,y) m = (m !! round y) !! round x == Terra

-- | __(1.c)__ A função 'verifConexaoBasePortais' verifica se existe, começando na base, um caminho de terra até a pelo menos um portal.
--
-- == Exemplos
--
-- >>> verifConexaoBasePortais (0,0) [(2,3)] (filtraPosicoesTerra mapa01 (0,0))
-- True
--
-- >>> verifConexaoBasePortais (0,0) [(4,0)] (filtraPosicoesTerra mapa01 (0,0))
-- False
--
-- >>>verifConexaoBasePortais (0,0) [(2,3),(4,0)] (filtraPosicoesTerra mapa01 (0,0))
-- False

verifConexaoBasePortais :: 
 -- | Posição da base.
 Posicao -> 
 -- | Posições dos portais.
 [Posicao] -> 
 -- | Posições dos caminhos de terra.
 [Posicao] -> 
 -- | Resultado na forma 'True' se existe algum caminho (de terra) da base a um dos portais ou 'False' se não existe tal caminho.
 Bool
verifConexaoBasePortais _ _ [] = False
verifConexaoBasePortais posBase posicoesPortais posicoesTerra = or (map (`elem` posicoesDisponiveis) posicoesPortais)
    where posicoesDisponiveis = removeElementosRepetidos (posicoesPossiveisParaPortais posicoesTerra posBase)

-- | A função 'posicoesPossiveisParaPortais' determina todas as posições que um portal pode ocupar num mapa, isto é, apenas as posições correspondentes a caminhos de terra
-- que estão ligados à base.
--
-- == Exemplos
--
-- >>> posicoesPossiveisParaPortais (filtraPosicoesTerra mapa01 (0,0)) (0,0)
-- [(0.0,0.0),(1.0,0.0),(1.0,1.0),(1.0,2.0),(1.0,3.0),(2.0,3.0),(2.0,4.0),(1.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(5.0,3.0),(5.0,2.0),(1.0,4.0),(2.0,4.0),(2.0,3.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(5.0,3.0),(5.0,2.0)]
--
-- >>> posicoesPossiveisParaPortais (filtraPosicoesTerra mapa01 (0,0)) (4,0)
-- [(4.0,0.0),(4.0,1.0)]

posicoesPossiveisParaPortais ::
 -- | Posições de terra do mapa.
 [Posicao] ->
 -- | Posição da base.
 Posicao ->
 -- | Posições possíveis para portais.
 [Posicao]
posicoesPossiveisParaPortais posicoesTerra posBase
    | null pontosAdj = [posBase]
    | compPontosAdj == 1 = posBase : posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (head pontosAdj)
    | compPontosAdj == 2 = posBase : posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (head pontosAdj) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (last pontosAdj)
    | compPontosAdj == 3 = posBase : posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (head pontosAdj) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (pontosAdj !! 1) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (last pontosAdj)
    | compPontosAdj == 4 = posBase : posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (head pontosAdj) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (pontosAdj !! 1) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (pontosAdj !! 2) ++
                           posicoesPossiveisParaPortais (removeElemento posBase posicoesTerra) (last pontosAdj)
        where pontosAdj = encontraPontosAdjacentes posBase posicoesTerra
              compPontosAdj = length pontosAdj

-- | __(1.d)__ A função 'verifSobreposPortaisTorresBase' verifica se há sobreposição dos portais com as torres ou com a base.
--
-- == Exemplos
--
-- >>> verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2)] (3,4)
-- True
--
-- >>> verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2)] (3,3)
-- False
--
-- >>> verifSobreposPortaisTorresBase [(0,0)] [(1,1),(2,2)] (3,3)
-- True
--
-- >>> verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(2,2),(0,0)] (3,4)
-- False
--
-- >>> verifSobreposPortaisTorresBase [(3,3)] [(1,1),(2,2)] (3,4)
-- True
--
-- >>> verifSobreposPortaisTorresBase [(0,0),(3,3)] [(1,1),(3,3)] (0,0)
-- False

verifSobreposPortaisTorresBase :: 
 -- | Posições dos portais.
 [Posicao] -> 
 -- | Posições das torres.
 [Posicao] ->
 -- | Posição da base.
 Posicao ->
 -- | Resultado na forma 'True' se não há qualquer sobreposição ou 'False' se há alguma sobreposição.
 Bool
verifSobreposPortaisTorresBase [] _ _ = True
verifSobreposPortaisTorresBase (p:ps) torres base
    | p `elem` bt = False
    | otherwise = verifSobreposPortaisTorresBase ps torres base
        where bt = base : torres

-- | __(1.e)__ A função 'verifOndasAtivasPorPortal' verifica se há, no máximo, uma onda ativa para um dado portal.
--
-- == Exemplos
--
-- >>> verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 5,Onda [Inimigo (1,1) Este 10 10 10 10 [] [] 10 1] 10 10 10])
-- True
--
-- >>> verifOndasAtivasPorPortal (Portal (0,0) [Onda [] 5 5 (-5),Onda [] 10 10 (-10)])
-- True
--
-- >>> verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 (-5),Onda [] 10 10 (-10)]) ~=? True
-- True
--
-- >>> verifOndasAtivasPorPortal (Portal (0,0) [Onda [Inimigo (0,0) Norte 5 5 5 5 [] [] 5 1] 5 5 (-5),Onda [Inimigo (1,1) Este 10 10 10 10 [] [] 10 1] 10 10 (-10)])
-- False

verifOndasAtivasPorPortal :: 
 -- | Portal de escolha.
 Portal ->
 -- | Resultado na forma 'True' se não há mais que uma onda ativa para o portal escolhido ou 'False' caso contrário.
 Bool
verifOndasAtivasPorPortal portal = verifOndasAtivasPorPortalAux ondas
        where ondas = ondasPortal portal
              verifOndaAtiva onda = not (null (inimigosOnda onda)) && entradaOnda onda < 0
              verifOndasAtivasPorPortalAux [] = True
              verifOndasAtivasPorPortalAux onds = length (filter verifOndaAtiva onds) <= 1





-- {- Funções relativas aos inimigos -} --

-- | __(2.a)__ A função 'verifInimigosInativos' verifica se os inimigos inativos (ainda por sair do portal) estão nas condições corretas, isto é,
-- estão na posição do seu portal, possuem vida positiva e não possuem projéteis ativos.
--
-- == Exemplos
--
-- >>> verifInimigosInativos (Portal (2,2) [])
-- True
--
-- >>> verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 1 1 1 1 [] [] 1 1] 1 1 1])
-- True
--
-- >>> verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,3) Norte 1 1 1 1 [] [] 1 1] 1 1 1])
-- False
--
-- >>> verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 0 1 1 1 [] [] 1 1] 1 1 1])
-- False
--
-- >>> verifInimigosInativos (Portal (2,2) [Onda [Inimigo (2,2) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1] 1 1 1])
-- False

verifInimigosInativos :: 
 -- | Portal do qual os inimigos irão sair.
 Portal ->
 -- | Resultado na forma 'True' se todas as condições são verificadas ou 'False' se pelo menos uma não está correta.
 Bool
verifInimigosInativos portal = verifPosicaoInimigoPortal inimigosInativos posPortal && vidaInimigoPositiva inimigosInativos && listaProjeteisAtivosInimigos inimigosInativos
    where posPortal = posicaoPortal portal
          inimigosInativos = map inimigosOnda (ondasPortal portal)

-- | A função 'verifPosicaoInimigoPortal' verifica se as posições dos inimigos de um portal coincidem com a posição do próprio portal.
--
-- == Exemplos
--
-- >>> verifPosicaoInimigoPortal [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]] (1,1)
-- True
--
-- >>> verifPosicaoInimigoPortal [[Inimigo (1,2) Norte 1 1 1 1 [] [] 1 1]] (1,1)
-- False
--
-- >>> verifPosicaoInimigoPortal [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1],[Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]] (1,1)
-- False

verifPosicaoInimigoPortal :: 
 -- | Inimigos do portal.
 [[Inimigo]] -> 
 -- | Posição do portal.
 Posicao -> 
 -- | Resultado na forma 'True' caso todos os inimigos estão na posição do seu portal ou 'False' caso contrário.
 Bool
verifPosicaoInimigoPortal [] _ = True
verifPosicaoInimigoPortal (li:lsi) pos = and (map (pos ==) (map posicaoInimigo li)) && verifPosicaoInimigoPortal lsi pos

-- | A função 'vidaInimigoPositiva' verifica se todos os inimigos de um portal possuem vida positiva.
--
-- == Exemplos
--
-- >>> vidaInimigoPositiva [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]]
-- True
--
-- >>>  vidaInimigoPositiva [[Inimigo (1,1) Norte (-1) 1 1 1 [] [] 1 1]] ~=? False
-- False
--
-- >>> vidaInimigoPositiva [[Inimigo (1,1) Norte (-1) 1 1 1 [] [] 1 1],[Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]]
-- False

vidaInimigoPositiva :: 
 -- | Inimigos do portal.
 [[Inimigo]] -> 
 -- | Resultado na forma 'True' se nenhum inimigo possui vida não positiva ou 'False' se pelo menos um não respeita a condição.
 Bool
vidaInimigoPositiva [] = True
vidaInimigoPositiva (li:lsi) = and (map (> 0) (map vidaInimigo li)) && vidaInimigoPositiva lsi

-- | A função 'listaProjeteisAtivosInimigos' verifica se nenhum inimigo num portal possui projéteis ativos.
--
-- == Exemplos
--
-- >>> listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]]
-- True
--
-- >>> listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1]]
-- False
--
-- >>> listaProjeteisAtivosInimigos [[Inimigo (1,1) Norte 1 1 1 1 [Projetil Fogo (Finita 1)] [] 1 1],[Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1]]
-- False

listaProjeteisAtivosInimigos :: 
 -- | Inimigos do portal.
 [[Inimigo]] -> 
 -- | Resultado na forma 'True' se nenhum inimgo possui projéteis ativos ou 'False' caso contrário.
 Bool
listaProjeteisAtivosInimigos [] = True
listaProjeteisAtivosInimigos (li:lsi) = and (map null (map projeteisInimigo li)) && listaProjeteisAtivosInimigos lsi

-- | __(2.b)__ A função 'todosInimigosSobreTerra' verifica se todos os inimigos ativos no jogo estão posicionados sobre terreno de terra.
--
-- == Exemplos
--
-- >>> todosInimigosSobreTerra [(1,1),(2,3)] (filtraPosicoesTerra mapa01 (0,0))
-- True
--
-- >>> todosInimigosSobreTerra [(1,1),(2,3),(2,0)] (filtraPosicoesTerra mapa01 (0,0))
-- False

todosInimigosSobreTerra :: 
 -- | Posições dos inimigos.
 [Posicao] ->
 -- | Posições de terreno de terra.
 [Posicao] ->
 -- | Resultado na forma 'True' se todos os inimigos estão posicionados sobre terra ou 'False' caso contrário.
 Bool
todosInimigosSobreTerra [] _ = True
todosInimigosSobreTerra (pi:pis) pst
    | pi `elem` pst = todosInimigosSobreTerra pis pst
    | otherwise = False

-- | __(2.c)__ A função 'inimigosSobreposTorres' verifica se nenhum inimigo se encontra posicionado sobre uma torre.
--
-- == Exemplos
--
-- >>> inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1]
-- True
--
-- >>> inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1] [Torre (2,2) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1]
-- False
--
-- >>> inimigosSobreposTorres [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,0) Norte 2 2 2 2 [] [] 2 1] [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1]
-- False

inimigosSobreposTorres :: 
 -- | Lista de inimigos.
 [Inimigo] ->
 -- | Lista de torres.
 [Torre] ->
 -- | Resultado na forma 'True' se nenhum inimigo se encontra posicionado sobre uma torre ou 'False' caso contrário.
 Bool
inimigosSobreposTorres [] _ = True
inimigosSobreposTorres (i:is) torres
    | pi `elem` pst = False
    | otherwise = inimigosSobreposTorres is torres
        where pi = posicaoInimigo i
              pst = map posicaoTorre torres

-- | __(2.d)__ A função 'velocidadeInimigosNaoNegativa' verifica se nenhum inimigo possui velocidade negativa.
--
-- == Exemplos
--
-- >>> velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 1 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]
-- True
--
-- >>> velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 0 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]
-- True
--
-- >>> velocidadeInimigosNaoNegativa [Inimigo (1,1) Norte 1 (-1) 1 1 [] [] 1 1,Inimigo (2,2) Norte 2 2 2 2 [] [] 2 1]
-- False

velocidadeInimigosNaoNegativa :: 
 -- | Lista de Inimigos
 [Inimigo] ->
 -- | Resultado na forma 'True' caso todos os inimigos possuem velocidade não negativa ou 'False' caso contrário.
 Bool
velocidadeInimigosNaoNegativa inimigos = and (map (>= 0) velocidadesInimigos)
    where velocidadesInimigos = map velocidadeInimigo inimigos

-- | __(2.e)__ A função 'verifProjeteisNormalizados' verifica se os projéteis de uma lista estão normalizados, ou seja, não pode haver vários projéteis do mesmo tipo
-- e não pode haver, simultâneamente, projéteis de gelo e fogo nem de resina e fogo.
--
-- == Exemplos
--
-- >>> verifProjeteisNormalizados [Projetil Fogo Infinita]
-- True
--
-- >>> verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Fogo (Finita 1)]
-- False
--
-- >>> verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Gelo (Finita 1)]
-- False
--
-- >>> verifProjeteisNormalizados [Projetil Fogo Infinita,Projetil Resina Infinita]
-- False
--
-- >>> verifProjeteisNormalizados [Projetil Gelo Infinita,Projetil Resina Infinita]
-- True
--
-- >>> verifProjeteisNormalizados [Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita]
-- True

verifProjeteisNormalizados :: 
 -- | Lista de projéteis.
 [Projetil] ->
 -- | Resultado na forma 'True' se os projéteis estão normalizados ou 'False' caso contrário.
 Bool
verifProjeteisNormalizados [] = True
verifProjeteisNormalizados projeteis
    | null tps = True
    | tp == Fogo && ((Fogo `elem` tps) || (Gelo `elem` tps) || (Resina `elem` tps)) = False
    | tp == Gelo && (Fogo `elem` tps || Gelo `elem` tps) = False
    | tp == Resina && (Fogo `elem` tps || Resina `elem` tps) = False
    | otherwise = verifProjeteisNormalizados (tail projeteis)
        where (tp:tps) = map tipoProjetil projeteis





-- {- Funções relativas às torres -} --

-- | __(3.a)__ A função 'verifTorresSobreRelva' verifica se todas as torres estão posicionadas em terreno de relva.
--
-- == Exemplos
--
-- >>> verifTorresSobreRelva [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1] mapa01
-- True
--
-- >>> verifTorresSobreRelva [Torre (1,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1] mapa01
-- False
--
-- >>> verifTorresSobreRelva [Torre (2,0) 1 1 1 1 1 (Projetil Gelo Infinita) 1 1,Torre (1,0) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1] mapa01
-- False

verifTorresSobreRelva ::
 -- | Lista de torres.
 [Torre] ->
 -- | Mapa do jogo.
 Mapa ->
 -- | Resultado na forma 'True' caso todas as torres estão posicionadas sobre relva ou 'False' caso contrário.
 Bool
verifTorresSobreRelva [] _ = True
verifTorresSobreRelva torres mapa
    | pt `elem` pr = verifTorresSobreRelva (tail torres) mapa
    | otherwise = False
        where pt = posicaoTorre (head torres)
              pr = filtraPosicoesRelva mapa (0,0)

-- | __(3.b)__ A função 'verifAlcanceTorres' verifica se todas as torres possuem um alcance não negativo.
--
-- == Exemplos
--
-- >>> verifAlcanceTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1]
-- True
--
-- >>> verifAlcanceTorres [Torre (1,1) 1 0 1 1 1 (Projetil Resina Infinita) 1 1]
-- True
--
-- >>> verifAlcanceTorres [Torre (1,1) 1 (-1) 1 1 1 (Projetil Resina Infinita) 1 1]
-- False

verifAlcanceTorres ::
 -- | Lista de torres.
 [Torre] ->
 -- | Resultado na forma 'True' se nenhuma torre possui alcance negativo ou 'False' se pelo menos uma possui alcance negativo.
 Bool
verifAlcanceTorres torres = and (map (>= 0) alcanceTorres)
    where alcanceTorres = map alcanceTorre torres

-- | __(3.c)__ A função 'verifRajadaTorres' verifica se todas as torres possuem uma rajada positiva.
--
-- == Exemplos
--
-- >>> verifRajadaTorres [Torre (1,1) 1 1 1 1 1 (Projetil Fraqueza Infinita) 1 1]
-- True
--
-- >>> verifRajadaTorres [Torre (1,1) 1 1 0 1 1 (Projetil Fraqueza Infinita) 1 1]
-- False
--
-- >>> verifRajadaTorres [Torre (1,1) 1 1 (-1) 1 1 (Projetil Fraqueza Infinita) 1 1]
-- False

verifRajadaTorres ::
 -- | Lista de torres.
 [Torre] ->
 -- | Resultado na forma 'True' todas as torres possuem rajada positiva ou 'False' se pelo menos uma possui rajada não positiva.
 Bool
verifRajadaTorres torres = and (map (> 0) rajadaTorres)
    where rajadaTorres = map rajadaTorre torres

-- | __(3.d)__ A função 'verifCicloTorres' verifica se todas as torres possuem um ciclo não negativo.
--
-- == Exemplos
--
-- >>> verifCicloTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1]
-- True
--
-- >>> verifCicloTorres [Torre (1,1) 1 1 1 0 1 (Projetil Resina Infinita) 1 1]
-- True
--
-- >>> verifCicloTorres [Torre (1,1) 1 1 1 (-1) 1 (Projetil Resina Infinita) 1 1]
-- False

verifCicloTorres ::
 -- | Lista de torres.
 [Torre] ->
 -- | Resultado na forma 'True' se nenhuma torre possui ciclo negativo ou 'False' se pelo menos uma possui ciclo negativo.
 Bool
verifCicloTorres torres = and (map (>= 0) cicloTorres)
    where cicloTorres = map cicloTorre torres

-- | __(3.e)__ A função 'verifSobreposTorres' verifica se as torres não estão sobrepostas.
--
-- == Exemplos
--
-- >>> verifSobreposTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1,Torre (2,2) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1]
-- True
--
-- >>> verifSobreposTorres [Torre (1,1) 1 1 1 1 1 (Projetil Resina Infinita) 1 1,Torre (1,1) 1 1 1 1 1 (Projetil Fogo Infinita) 1 1]
-- False

verifSobreposTorres ::
 -- | Lista de torres.
 [Torre] ->
 -- | Resultado na forma 'True' se não há sobreposição de torres ou 'False' caso contrário.
 Bool
verifSobreposTorres [] = True
verifSobreposTorres torres
    | pt `elem` pts = False
    | otherwise = verifSobreposTorres (tail torres)
        where (pt:pts) = map posicaoTorre torres





-- {- Funções relativas à base -} --

-- | __(4.a)__ A função 'verifBaseSobreTerra' verifica se a base está posicionada sobre terra.
--
-- == Exemplos
--
-- >>> verifBaseSobreTerra (Base 1 (0,0) 1) mapa01
-- True
--
-- >>> verifBaseSobreTerra (Base 1 (2,2) 1) mapa01
-- False

verifBaseSobreTerra :: 
 -- | Base do jogo.
 Base ->
 -- | Mapa do jogo.
 Mapa ->
 -- | Resultado na forma 'True' se a base está posicionada sobre terra ou 'False' caso contrário.
 Bool
verifBaseSobreTerra base mapa = posicaoBase base `elem` pt
    where pt = filtraPosicoesTerra mapa (0,0)

-- | __(4.b)__ A função 'verifCreditosBase' verifica se os créditos da base não são negativos.
--
-- == Exemplos
--
-- >>> verifCreditosBase (Base 1 (0,0) 1)
-- True
--
-- >>> verifCreditosBase (Base 1 (0,0) 0)
-- True
--
-- >>> verifCreditosBase (Base 1 (0,0) (-1))
-- False

verifCreditosBase :: 
 -- | Base do jogo.
 Base ->
 -- | Resultado na forma 'True' se os créditos da base não são negativos ou 'False' se são negativos.
 Bool
verifCreditosBase base = creditosBase base >= 0

-- | __(4.c)__ A função 'verifSobrepoisBaseTorresPortais' verifica se a base não está sobreposta com torres ou com portais.
--
-- == Exemplos
--
-- >>> verifSobreposBaseTorresPortais (1,1) [(0,0),(2,2)] [(3,3),(4,4)]
-- True
--
-- >>> verifSobreposBaseTorresPortais (1,1) [(0,0),(1,1)] [(3,3),(4,4)]
-- False
--
-- >>> verifSobreposBaseTorresPortais (1,1) [(0,0),(2,2)] [(1,1),(4,4)]
-- False
--
-- >>> verifSobreposBaseTorresPortais (1,1) [(1,1),(2,2)] [(1,1),(4,4)]
-- False

verifSobreposBaseTorresPortais :: 
 -- | Posição da base.
 Posicao ->
 -- | Posições das torres.
 [Posicao] ->
 -- | Posições dos portais.
 [Posicao] ->
 -- | Resultado na forma 'True' caso não haja sobreposições ou 'False' caso contrário.
 Bool
verifSobreposBaseTorresPortais posBase posTorres posPortais = posBase `notElem` posTorres && posBase `notElem` posPortais