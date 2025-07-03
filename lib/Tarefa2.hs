{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use map once" #-}
{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Bruno Miguel Silva Magalhães <a110274@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}

module Tarefa2 where

import LI12425
import Tarefa1





-- {- Funções auxiliares -} --

-- | A função 'inimigoEstaNoAlcance' verifica se um dado inimigo está no alcance de uma torre.
--
-- == Exemplos
--
-- >>> inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (5,4) Norte 50 50 50 50 [] [] 50 1)
-- True
--
-- >>> inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1)
-- True
--
-- >>> inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (9,4) Norte 50 50 50 50 [] [] 50 1)
-- False
--
-- >>> inimigoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (8,8) Norte 50 50 50 50 [] [] 50 1)
-- False

inimigoEstaNoAlcance ::
 -- | Torre de escolha.
 Torre ->
 -- | Inimigo de escolha.
 Inimigo ->
 -- | Resultado na forma 'True' caso o inimigo esteja no alcance da torre ou 'False' caso contrário.
 Bool
inimigoEstaNoAlcance torre inimigo = dist (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre

-- | A função 'somaDuracoes' recebe dois projéteis e devolve um projétil cuja duração é a soma das durações dos projéteis iniciais, mantendo o seu tipo.
-- Esta função apenas funciona corretamente se os projéteis são do mesmo tipo.
--
-- == Exemplos
--
-- >>> somaDuracoes (Projetil Fogo (Finita 4)) (Projetil Fogo (Finita 5))
-- Projetil Fogo (Finita 9)
--
-- >>> somaDuracoes (Projetil Gelo Infinita) (Projetil Gelo (Finita 5))
-- Projetil Gelo Infinita
--
-- >>> somaDuracoes (Projetil Resina (Finita 5)) (Projetil Resina Infinita)
-- Projetil Resina Infinita
--
-- >>> somaDuracoes (Projetil Fraqueza Infinita) (Projetil Fraqueza Infinita)
-- Projetil Fraqueza Infinita

somaDuracoes :: 
 -- | Primeiro projétil.
 Projetil ->
 -- | Segundo projétil (do mesmo tipo que o primeiro).
 Projetil ->
 -- | Projétil resultante.
 Projetil
somaDuracoes (Projetil t Infinita) _ = Projetil t Infinita
somaDuracoes _ (Projetil t Infinita) = Projetil t Infinita
somaDuracoes (Projetil t (Finita d1)) (Projetil _ (Finita d2)) = Projetil t (Finita (d1 + d2))

-- | A função 'verifFraqueza' verifica se um projétil é do tipo Fraqueza.
--
-- == Exemplos
--
-- >>> verifFraqueza (Projetil Fraqueza Infinita)
-- True
--
-- >>> verifFraqueza (Projetil Fraqueza (Finita 3))
-- True
--
-- >>> verifFraqueza (Projetil Fogo Infinita)
-- False

verifFraqueza ::
 -- | Projétil recebido.
 Projetil ->
 -- | Resultado no tipo 'True' caso o projétil seja de fraqueza ou 'False' caso contrário.
 Bool
verifFraqueza (Projetil Fraqueza _) = True
verifFraqueza _ = False

-- | A função 'verifNaoFraqueza' verifica se um projétil não é do tipo fraqueza.
--
-- == Exemplos
--
-- >>> verifNaoFraqueza (Projetil Fraqueza Infinita)
-- False
--
-- >>> verifNaoFraqueza (Projetil Fraqueza (Finita 3))
-- False
--
-- >>> verifNaoFraqueza (Projetil Fogo Infinita)
-- True

verifNaoFraqueza ::
 -- | Projétil recebido.
 Projetil ->
 -- | Resultado no tipo 'False' caso o projétil seja de fraqueza ou 'True' caso contrário.
 Bool
verifNaoFraqueza projs = not (verifFraqueza projs)

-- | A função 'normalizaProjeteis' normaliza os projéteis de uma lista.
--
-- == Exemplos
--
-- >>> normalizaProjeteis [Projetil Fraqueza (Finita 5),Projetil Fraqueza Infinita,Projetil Gelo Infinita,Projetil Fogo Infinita]
-- [Projetil Fraqueza Infinita]
--
-- >>> normalizaProjeteis [Projetil Fraqueza (Finita 5),Projetil Fraqueza (Finita 1),Projetil Resina Infinita,Projetil Fogo (Finita 4)]
-- [Projetil Fraqueza (Finita 6),Projetil Fogo (Finita 8)]

normalizaProjeteis ::
 -- | Lista inicial de projéteis.
 [Projetil] ->
 -- | Lista de projéteis normalizados.
 [Projetil]
normalizaProjeteis projs
    | null projeteisFraqueza = normalizaProjeteisNaoFraqueza (filter verifNaoFraqueza projs)
    | length projeteisFraqueza == 1 = projeteisFraqueza ++ normalizaProjeteisNaoFraqueza (filter verifNaoFraqueza projs)
    | otherwise = somaDuracoes (head projeteisFraqueza) (last projeteisFraqueza) : normalizaProjeteisNaoFraqueza (filter verifNaoFraqueza projs)
        where projeteisFraqueza = filter verifFraqueza projs

-- | A função 'normalizaProjeteis' normaliza os projéteis (que não são de fraqueza) de uma lista.
--
-- == Exemplos
--
-- >>> normalizaProjeteisNaoFraqueza [Projetil Gelo Infinita,Projetil Fogo Infinita]
-- []
--
-- >>> normalizaProjeteisNaoFraqueza [Projetil Resina (Finita 1),Projetil Fogo (Finita 3)]
-- [Projetil Fogo (Finita 6)]

normalizaProjeteisNaoFraqueza :: 
 -- | Lista inicial de projéteis (que não são de fraqueza).
 [Projetil] ->
 -- | Lista de projéteis normalizados.
 [Projetil]
normalizaProjeteisNaoFraqueza [] = []
normalizaProjeteisNaoFraqueza [projetil] = [projetil]
normalizaProjeteisNaoFraqueza (proj1 : proj2 : projs)
    | tipoProj1 == tipoProj2 = normalizaProjeteisNaoFraqueza (somaDuracoes proj1 proj2 : projs)
    | (tipoProj1 == Fogo && tipoProj2 == Gelo) || (tipoProj1 == Gelo && tipoProj2 == Fogo) = normalizaProjeteisNaoFraqueza projs
    | tipoProj1 == Fogo && tipoProj2 == Resina = normalizaProjeteisNaoFraqueza (somaDuracoes proj1 proj1 : projs)
    | tipoProj1 == Resina && tipoProj2 == Fogo = normalizaProjeteisNaoFraqueza (somaDuracoes proj2 proj2 : projs)
    | otherwise = proj1 : proj2 : projs
        where tipoProj1 = tipoProjetil proj1
              tipoProj2 = tipoProjetil proj2



-- | A função 'novosProjeteisInimigo' calcula o que acontece à lista de projéteis de um inimigo quando atingido por outro projétil.
--
-- == Exemplos
--
-- >>> novosProjeteisInimigo (Projetil Fogo Infinita) [Projetil Gelo Infinita]
-- []
--
-- >>> novosProjeteisInimigo (Projetil Fraqueza (Finita 4)) [Projetil Resina Infinita,Projetil Fraqueza (Finita 6)]
-- [Projetil Fraqueza (Finita 10),Projetil Resina Infinita]

novosProjeteisInimigo :: 
 -- | Projétil que atinge o inimigo.
 Projetil ->
 -- | Lista de projéteis do inimigo.
 [Projetil] ->
 -- | Lista de projéteis resultante.
 [Projetil]
novosProjeteisInimigo proj projs = normalizaProjeteis (proj : projs)

-- | A função 'existOndaAtiva' verifica se exise pelo menos uma onda ativa numa lista de ondas.
--
-- == Exemplos
--
-- >>> existOndaAtiva []
-- False
--
-- >>> existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5]
-- False
--
-- >>> existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]
-- True
--
-- >>> existOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]
-- True
--
-- >>> existOndaAtiva [Onda [] 5 (-5) (-5)]
-- False

existOndaAtiva :: 
 -- | Lista de ondas.
 [Onda] ->
 -- | Resultado na forma 'True' se existe pelo menos uma onda ativa na lista ou 'False' caso contrário.
 Bool
existOndaAtiva [] = False
existOndaAtiva (o:os)
    | entradaOnda o <= 0 && not (null (inimigosOnda o)) = True
    | otherwise = existOndaAtiva os

-- | A função 'posicaoOndaAtiva' determina a posição da onda ativa (assumindo que esta existe) numa lista de ondas.
--
-- == Exemplos
--
-- >>> posicaoOndaAtiva [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]
-- 0
--
-- >>> posicaoOndaAtiva [Onda [] 40 (-5) (-5),Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]
-- 1

posicaoOndaAtiva :: 
 -- | Lista de ondas (lista que possui pelo menos uma onda ativa).
 [Onda] -> 
 -- | Posição da onda ativa na lista.
 Int
posicaoOndaAtiva (o:os)
    | entradaOnda o <= 0 && not (null (inimigosOnda o)) = 0
    | otherwise = 1 + posicaoOndaAtiva os





-- {- Funções principais -} --

-- | A função 'inimigosNoAlcance' determina todos os inimigos (ativos) que estão no alcance de uma torre.
--
-- == Exemplos
--
-- >>> inimigosNoAlcance (Torre (5,5) 5 2 5 5 5 (Projetil Fraqueza Infinita) 1 1) [Inimigo (2,3) Norte 40 40 40 40 [] [] 1 1,Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1]
-- [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1]
--
-- >>> inimigosNoAlcance (Torre (5,5) 5 2 5 5 5 (Projetil Fraqueza Infinita) 1 1) []
-- []

inimigosNoAlcance :: 
 -- | Torre de foco.
 Torre ->
 -- | Lista de inimigos ativos no jogo.
 [Inimigo] ->
 -- | Lista de inimigos ativos que estão no alcance da torre.
 [Inimigo]
inimigosNoAlcance torre = filter (inimigoEstaNoAlcance torre)

-- | A função 'atingeInimigo' calcula o resultado de um inimigo ser atingido por uma torre.
--
-- == Exemplos
--
-- >>> atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1) (Inimigo (2,3) Norte 40 40 40 40 [Projetil Gelo Infinita] [Fogo] 1 1)
-- Inimigo (2,3) Norte 35 40 40 40 [Projetil Gelo Infinita] [Fogo] 1 1
--
-- >>> atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1) (Inimigo (2,3) Norte 40 40 40 40 [Projetil Gelo Infinita] [] 1 1)
-- Inimigo (2,3) Norte 35 40 40 40 [] [] 1 1
--
-- >>> atingeInimigo (Torre (5,5) 5 2 5 5 5 (Projetil Fogo (Finita 4)) 1 1) (Inimigo (2,3) Norte 40 40 40 40 [Projetil Fraqueza Infinita] [] 1 1)
-- Inimigo (2,3) Norte 30 40 40 40 [Projetil Fraqueza Infinita,Projetil Fogo (Finita 4)] [] 1 1

atingeInimigo :: 
 -- | Torre de foco.
 Torre ->
 -- | Tempo do dia.
 Tempo ->
 -- | Tempo do jogo.
 Tempo ->
 -- | Inimigo atingido.
 Inimigo ->
 -- | Inimigo resultante.
 Inimigo
atingeInimigo t td tj i
    | mod (head (geraAleatorios (floor tj * 60) 1)) 100 < probErrar = i
    | any verifFraqueza projInimigo = Inimigo posInimigo dirInimigo (vidInimigo - 2 * danoTorre t * efeitoDiaNoite * efeitoCritico) velInimigo ataqInimigo butInimigo projeteisAtualizados imunidades vidMaxima num
    | otherwise = Inimigo posInimigo dirInimigo (vidInimigo - danoTorre t * efeitoDiaNoite * efeitoCritico) velInimigo ataqInimigo butInimigo projeteisAtualizados imunidades vidMaxima num
        where (Inimigo posInimigo dirInimigo vidInimigo velInimigo ataqInimigo butInimigo projInimigo imunidades vidMaxima num) = i
              projeteisAtualizados = if tipoProjetil (projetilTorre t) `elem` imunidades then projInimigo else novosProjeteisInimigo (projetilTorre t) projInimigo
              efeitoDiaNoite
                | td > 3 && td < 21 && num == 11 = 1.2
                | (td <= 3 || td >= 21) && (num == 3 || num == 8) = 0.8
                | otherwise = 1
              probErrar
                | td <= 3 || td >= 21 = probabilidadaErrar t * 2
                | otherwise = probabilidadaErrar t
              efeitoCritico
                | mod (head (geraAleatorios (floor tj * 60) 1)) 100 < probabilidadaCritico t = 2
                | otherwise = 1

-- | A função 'ativaInimigo' calcula o que acontece a um portal quando uma das suas ondas ativa um inimigo.
--
-- == Exemplos
--
-- >>> ativaInimigo (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) []
-- (Portal (4,4) [Onda [] 5 5 (-5)],[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1])
--
-- >>> ativaInimigo (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) []
-- (Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5,Onda [] 5 5 (-5)],[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1])

ativaInimigo :: 
 -- | Portal de foco.
 Portal ->
 -- | Lista de inimigos ativos.
 [Inimigo] ->
 -- | Resultado na forma '(portal resultante,lista de inimigos ativos resultante)'.
 (Portal,[Inimigo])
ativaInimigo portal inimigos
    | not (existOndaAtiva ondas) = (portal,inimigos)
    | tempOnda <= 0 = (Portal posPortal (take x ondas ++ Onda (tail inimOndas) cicOnda cicOnda entOnda : drop (x + 1) ondas),head inimOndas : inimigos)
    | otherwise = (portal,inimigos)
        where (Portal posPortal ondas) = portal
              x = posicaoOndaAtiva ondas
              (Onda inimOndas cicOnda tempOnda entOnda) = ondas !! x

-- | A função 'terminouJogo' verifica se o jogo terminou, isto é, se o jogador já perdeu ou já ganhou.
--
-- == Exemplos
--
-- >>> terminouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- True
--
-- >>> terminouJogo (Jogo (Base (-10) (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- True
--
-- >>> terminouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- False

terminouJogo :: 
 -- | Jogo atual.
 Jogo ->
 -- | Resultado na forma 'True' se o jogo já acabou ou 'False' caso contrário.
 Bool
terminouJogo j = ganhouJogo j || perdeuJogo j

-- | A função 'ganhouJogo' verifica se o jogador ganhou o jogo.
--
-- == Exemplos
--
-- >>> ganhouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- True
--
-- >>> ganhouJogo (Jogo (Base 50 (4,4) 50) [Portal (4,4) [Onda [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1] 5 5 5]] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- False

ganhouJogo :: 
 -- | Jogo atual.
 Jogo ->
 -- | Resultado na forma 'True' se o jogador já tiver ganho ou 'False' caso contrário.
 Bool
ganhouJogo (Jogo _ portais _ _ inimigos _ _ _ _ _ _) = null inimigos && and (map naoTemInimigos ondas)
    where ondas = map ondasPortal portais
          naoTemInimigos [] = True
          naoTemInimigos (o:os)
            | null (inimigosOnda o) = naoTemInimigos os
            | otherwise = False

-- | A função 'perdeuJogo' verifiica se o jogador perdeu o jogo.
--
-- == Exemplos
--
-- >>> perdeuJogo (Jogo (Base 50 (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- False
--
-- >>> perdeuJogo (Jogo (Base (-10) (4,4) 50) [] [] [] [] [] 1 (1,1) 0 (False,30,[]))
-- True

perdeuJogo :: 
 -- | Jogo atual.
 Jogo ->
 -- | Resultado na forma 'True' se o jogador já tiver perdido ou 'False' caso contrário.
 Bool
perdeuJogo (Jogo base _ _ _ _ _ _ _ _ _ _) = vidaBase base <= 0