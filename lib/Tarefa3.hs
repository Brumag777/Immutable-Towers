{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use any" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use uncurry" #-}
{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Bruno Miguel Silva Magalhães <a110274@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa2
import Tarefa1

-- | A função 'atualizaJogo' é responsável por atualizar tudo o que ocorre durante um jogo automaticamente, tal como o movimentos dos inimigos e os ataques das torres.

atualizaJogo ::
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo de jogo.
 Tempo ->
 -- | Jogo pré-atualização.
 Jogo ->
 -- | Jogo atualizado.
 Jogo
atualizaJogo t tj jogo = Jogo baseAtualizada portaisAtualizados torresAtualizadas mapa 
                              (atualizaEfeitosChuva (atualizaVidaInimigosLagrimasCristalinas lagrimasCristalinas inimigosAtualizados' t ec) numero b t) 
                              loja numero posicaoTec q' ec' td'
                                where torres = torresJogo jogo
                                      portais = portaisJogo jogo
                                      mapa = mapaJogo jogo
                                      inimigos = inimigosJogo jogo
                                      base = baseJogo jogo
                                      loja = lojaJogo jogo
                                      numero = numeroNivel jogo
                                      posicaoTec = posicaoTeclado jogo
                                      q = inimigosDerrotados jogo
                                      ec = estadoChuva jogo
                                      (b,_,_) = ec
                                      td = tempoDia jogo
                                      ec' = atualizaGotasChuva ec (tj * 60) t
                                      (portaisAtualizados,todosInimigos) = atualizaPortais portais t inimigos
                                      (torresAtualizadas,inimigosAtualizados) = atualizaTorres torres todosInimigos t td tj
                                      (baseAtualizada,inimigosAtualizados',q') = atualizaInimigos inimigosAtualizados mapa base t td q b
                                      lagrimasCristalinas = filtraPosicoesLagrimasCristalinas mapa (0,0)
                                      td' = atualizaTempoDia td t

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
-- | 'mapaX' é um exemplo de um mapa.

mapaX :: Mapa
mapaX = [[t, t, t],
         [t, t, t],
         [t, t, t]]
            where t = Terra

-- | 'mapaY' é um exemplo de um mapa.

mapaY :: Mapa
mapaY = [[t, t, l],
         [t, l, l],
         [t, t, t]]
            where t = Terra
                  l = LagrimasCristalinas





-- {- Funções Auxiliares -} --

-- | A função 'atualizaEfeitosChuva' atualiza os inimigos com base nos efeitos da chuva.
--
-- == Exemplos
--
-- >>> atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 1 False 5
-- [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1]
--
-- >>> atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 1 True 5
-- [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1]
--
-- >>> atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo Infinita] [] 1 1] 2 True 5
-- [Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1]
--
-- >>> atualizaEfeitosChuva [Inimigo (4,4) Norte 40 40 40 40 [] [Fogo] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Gelo Infinita] [] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Resina Infinita] [] 1 1] 3 True 5
-- [Inimigo (4,4) Norte 40 40 40 40 [] [Fogo] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo (Finita 5)] [] 1 1,Inimigo (4,4) Norte 40 40 40 40 [Projetil Fogo (Finita 5)] [] 1 1]
--
-- >>> atualizaEfeitosChuva [Inimigo (4,4) Norte 30 40 40 40 [] [] 100 1,Inimigo (4,4) Norte 90 40 40 40 [] [] 100 1] 4 True 5
-- [Inimigo (4,4) Norte 80 40 40 40 [] [] 100 1,Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1]

atualizaEfeitosChuva :: 
 -- | Inimigos pré-atualização.
 [Inimigo] -> 
 -- | Número do nível do jogo.
 Int -> 
 -- | Se a chuva está ou não ativa.
 Bool -> 
 -- | Tempo a atualizar.
 Tempo ->
 -- | Inimigos pós-atualização.
 [Inimigo]
atualizaEfeitosChuva is n b t
    | not b = is
    | n == 3 || n == 7 = map (adicionaProjetilFogo t . removeProjetilResina. removeProjetilGelo) is
    | n == 4 || n == 8 = map (regeneraInimigoChuvaLagrimasCristalinas t) is
    | otherwise = map removeProjetilFogo is

-- | A função 'regeneraInimigoChuvaLagrimasCristalinas' regenera os inimigos quando está a chover lágrimas cristalinas.
--
-- == Exemplos
--
-- >>> regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 30 40 40 40 [] [] 100 1)
-- Inimigo (4,4) Norte 80 40 40 40 [] [] 100 1
--
-- >>> regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 90 40 40 40 [] [] 100 1)
-- Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1
--
-- >>> regeneraInimigoChuvaLagrimasCristalinas 5 (Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1)
-- Inimigo (4,4) Norte 100 40 40 40 [] [] 100 1

regeneraInimigoChuvaLagrimasCristalinas :: Tempo -> Inimigo -> Inimigo
regeneraInimigoChuvaLagrimasCristalinas t i = Inimigo pos dir (min (vida + 10 * t) vMax) velocidade ataque butim ps imunidades vMax n
    where Inimigo pos dir vida velocidade ataque butim ps imunidades vMax n = i

-- | A função 'filtraPosicoesLagrimasCristalinas' determina as coordenadas de pontos correnpondentes apenas ao terreno de 'LagrimasCristalinas' de um mapa.
--
-- == Exemplos
--
-- >>> filtraPosicoesLagrimasCristalinas mapaX (0,0)
-- []
--
-- >>> filtraPosicoesLagrimasCristalinas mapaY (0,0)
-- [(2,0),(1,1),(2,1)]

filtraPosicoesLagrimasCristalinas :: 
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição do ponto na matriz (deve ser (0,0) inicialmente para determinar corretamente as coordenadas).
 Posicao ->
 -- | Posições correpondentes ao terreno de 'LagrimasCristalinas'.
 [Posicao]
filtraPosicoesLagrimasCristalinas [] _ = []
filtraPosicoesLagrimasCristalinas (l:ls) (x,y) = filtraPosicoesLagrimasCristalinasAux l (0,y) ++ filtraPosicoesLagrimasCristalinas ls (x,y + 1)
    where filtraPosicoesLagrimasCristalinasAux [] _ = []
          filtraPosicoesLagrimasCristalinasAux (c:cs) (a,b)
            | c == LagrimasCristalinas = (a,b) : filtraPosicoesLagrimasCristalinasAux cs (a + 1,b)
            | otherwise = filtraPosicoesLagrimasCristalinasAux cs (a + 1,b)

-- | A função 'verifGelo' verifica se um projétil é do tipo Gelo.
--
-- == Exemplos
--
-- >>> verifGelo (Projetil Gelo Infinita)
-- True
--
-- >>> verifFogo (Projetil Resina Infinita)
-- False

verifGelo ::
 -- | Projétil recebido.
 Projetil ->
 -- | Resultado no tipo 'True' caso o projétil seja de Gelo ou 'False' caso contrário.
 Bool
verifGelo (Projetil Gelo _) = True
verifGelo _ = False

-- | A função 'verifFogo' verifica se um projétil é do tipo Fogo.
--
-- == Exemplos
--
-- >>> verifFogo (Projetil Fogo Infinita)
-- True
--
-- >>> verifFogo (Projetil Resina Infinita)
-- True

verifFogo ::
 -- | Projétil recebido.
 Projetil ->
 -- | Resultado no tipo 'True' caso o projétil seja de Fogo ou 'False' caso contrário.
 Bool
verifFogo (Projetil Fogo _) = True
verifFogo _ = False

-- | A função 'verifResina' verifica se um projétil é do tipo Resina.
--
-- == Exemplos
--
-- >>> verifResina (Projetil Resina Infinita)
-- True
--
-- >>> verifResina (Projetil Fraqueza Infinita)
-- False

verifResina ::
 -- | Projétil recebido.
 Projetil ->
 -- | Resultado no tipo 'True' caso o projétil seja de Resina ou 'False' caso contrário.
 Bool
verifResina (Projetil Resina _) = True
verifResina _ = False

-- | A função 'inimigoNaoEstaNoAlcance' verifica se um inimigo não está no alcance de uma torre.
--
-- == Exemplos
--
-- >>> inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (5,4) Norte 50 50 50 50 [] [] 50 1)
-- False
--
-- >>> inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1)
-- False
--
-- >>> inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (9,4) Norte 50 50 50 50 [] [] 50 1)
-- True
--
-- >>> inimigoNaoEstaNoAlcance (Torre (4,4) 5 4 5 5 5 (Projetil Fogo Infinita) 1 1) (Inimigo (8,8) Norte 50 50 50 50 [] [] 50 1)
-- True

inimigoNaoEstaNoAlcance ::
 -- | Torre de escolha. 
 Torre ->
 -- | Inimigo de escoha.
 Inimigo ->
 -- | Resultado na forma 'True' caso o inimigo não esteja no alcance da torre ou 'False' caso contrário.
 Bool
inimigoNaoEstaNoAlcance torre inimigo = not (inimigoEstaNoAlcance torre inimigo)

-- | A função 'detetaInimigos' realiza os ataques de uma torre aos inimigos que estão no seu alcance (quantidade máxima correspondente à sua rajada) caso esta esteja pronta para atacar.
--
-- == Exemplo
--
-- >>> detetaInimigos (Torre (4,4) 5 4 2 5 (-5) (Projetil Fogo Infinita) 1 1) [Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1] 5
-- (Torre (4,4) 5 4 2 5 5 (Projetil Fogo Infinita) 1 1,[Inimigo (5,5) Norte 45 50 50 50 [Projetil Fogo Infinita] [] 50 1,Inimigo (5,5) Norte 45 50 50 50 [Projetil Fogo Infinita] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1])]

detetaInimigos ::
 -- | Torre a atualizar.
 Torre ->
 -- | Inimigos ativos que se encontram no alcance da torre.
 [Inimigo] -> 
 -- | Tempo a atualizar.
 Tempo -> 
 -- | Tempo do dia.
 Tempo ->
 -- | Tempo do jogo.
 Tempo ->
 -- | Resultado com a torre e os inimigos atualizados.
 (Torre,[Inimigo])
detetaInimigos torre is t td tj
    | tempo <= 0 && not (null is) = (Torre posicao dano alcance rajada ciclo ciclo projetil prob1 prob2 nivel numero,map (atingeInimigo torre td tj) (take rajada is) ++ drop rajada is)
    | otherwise = (Torre posicao dano alcance rajada ciclo (tempo - t) projetil prob1 prob2 nivel numero,is)
        where Torre posicao dano alcance rajada ciclo tempo projetil prob1 prob2 nivel numero = torre

-- | A função 'removeInimigosMortos' remove os inimigos mortos do jogo e, como consequência, altera os créditos da base.
--
-- == Exemplo
--
-- >>> removeInimigosMortos [Inimigo (5,5) Norte (-10) 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 0 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1] (Base 50 (4,4) 50) 10 1
-- (Base 100 (4,4) 100,[Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1])]

removeInimigosMortos ::
 -- | Lista de inimigos.
 [Inimigo] ->
 -- | Base do jogo.
 Base ->
 -- | Quantidade de inimigos derrotados.
 Int ->
 -- | Resultado com a base atualizada e os inimigos com vida positiva (vivos).
 (Base,[Inimigo],Int)
removeInimigosMortos [] base q = (base,[],q)
removeInimigosMortos (i:is) base q
    | vidaInimigo i <= 0 = removeInimigosMortos is (Base vidaBase posBase (credBase + butimInimigo i)) (q + 1)
    | vidaInimigo i >= 0 = let (b',i',q') = removeInimigosMortos is base q in (b',i:i',q')
        where Base vidaBase posBase credBase = base

-- | A função 'inimigosAtingemBase' remove os inimigos que já alcançaram a base e danifica a mesma como consequência.
--
-- == Exemplo
--
-- >>> [inimigosAtingemBase (Base 200 (4,4) 100) [Inimigo (4.25,4) Norte 50 50 50 50 [] [] 50 1,Inimigo (4.25,4) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1]
-- (Base 100 (4,4) 100,[Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1,Inimigo (5,5) Norte 50 50 50 50 [] [] 50 1])]

inimigosAtingemBase ::
 -- | Base do jogo.
 Base ->
 -- | Lista de inimigos.
 [Inimigo] ->
 -- | Resultado com a base atualizada e os inimigos que ainda não atingiram a base.
 (Base,[Inimigo])
inimigosAtingemBase base [] = (base,[])
inimigosAtingemBase (Base vidaB posB c) (i:is)
    | dist (posicaoInimigo i) posB <= 0.5 = inimigosAtingemBase (Base (vidaB - ataqueInimigo i) posB c) is
    | otherwise = let (b',i') = inimigosAtingemBase (Base vidaB posB c) is in (b',i : i')

-- | A função 'atualizaPosicao' atualiza a posição de um inimigo.
--
-- == Exemplos
--
-- >>> atualizaPosicao Norte (4,4) 3 1
-- (4,1)
--
-- >>> atualizaPosicao Sul (4,4) 3 1
-- (4,7)
--
-- >>> atualizaPosicao Este (4,4) 3 1
-- (7,4)
--
-- >>> atualizaPosicao Oeste (4,4) 3 1
-- (1,4)

atualizaPosicao ::
 -- | Direção do inimigo.
 Direcao ->
 -- | Posição do inimigo.
 Posicao ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Velocidade do inimigo (afetada pelos projéteis).
 Float ->
 -- | Posição do inimigo atualizada.
 Posicao
atualizaPosicao d (x,y) t v = case d of
    Norte -> (x,y - v * t)
    Sul -> (x,y + v * t)
    Este -> (x + v * t,y)
    Oeste -> (x - v * t,y)

-- | A função 'divFloat' dá a parte decimal de um número positivo.
--
-- == Exemplo
--
-- >>> divFloat 4.5
-- 0.5

divFloat ::
 -- | Número positivo.
 Float ->
 -- | Parte decimal do número.
 Float
divFloat x
    | x >= 1 = divFloat (x - 1)
    | x <= 1 = x

-- | A função 'atualizaDirecao' atualiza a direção de um inimigo de forma a que não o permita sair dos caminhos de terra (a direção do inimigo é atualizada apenas quando este se
-- encontra na região central de uma terra, pelo que todos os inimigos devem começar no centro de uma terra).
--
-- == Exemplos
--
-- >>> atualizaDirecao Norte (0.5,0.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),(2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)]
-- Este
--
-- >>> atualizaDirecao Este (1.5,0.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),(2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)]
-- Sul
--
-- >>> atualizaDirecao Sul (1.5,4.5) (5,2) [(0.0,0.0),(1.0,0.0),(4.0,0.0),(1.0,1.0),(4.0,1.0),(1.0,2.0),(5.0,2.0),(1.0,3.0),(2.0,3.0),(5.0,3.0),(1.0,4.0),(2.0,4.0),(3.0,4.0),(4.0,4.0),(5.0,4.0),(1.0,5.0)]
-- Este

atualizaDirecao ::
 -- | Direção do inimigo.
 Direcao ->
 -- | Posição do inimigo.
 Posicao ->
 -- | Posição da base.
 Posicao ->
 -- | Lista de posições dos caminhos de terra do mapa.
 [Posicao] ->
 -- | Direção do inimigo atualizada.
 Direcao
atualizaDirecao direcao (x,y) (xb,yb) terras
    | divFloat x > 0.4 && divFloat x < 0.6 && divFloat y > 0.4 && divFloat y < 0.6 = case direcao of
    Norte -> if (fromInteger (floor x),fromInteger (floor y - 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y - 1)) `elem` posicoesPosiveis then Norte
             else if (fromInteger (floor x - 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x - 1),fromInteger (floor y)) `elem` posicoesPosiveis then Oeste
             else if (fromInteger (floor x + 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x + 1),fromInteger (floor y)) `elem` posicoesPosiveis then Este
             else Sul
    Sul -> if (fromInteger (floor x),fromInteger (floor y + 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y + 1)) `elem` posicoesPosiveis then Sul
           else if (fromInteger (floor x - 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x - 1),fromInteger (floor y)) `elem` posicoesPosiveis then Oeste
           else if (fromInteger (floor x + 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x + 1),fromInteger (floor y)) `elem` posicoesPosiveis then Este
           else Norte
    Este -> if (fromInteger (floor x + 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x + 1),fromInteger (floor y)) `elem` posicoesPosiveis then Este
            else if (fromInteger (floor x),fromInteger (floor y - 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y - 1)) `elem` posicoesPosiveis then Norte
            else if (fromInteger (floor x),fromInteger (floor y + 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y + 1)) `elem` posicoesPosiveis then Sul
            else Oeste
    Oeste -> if (fromInteger (floor x - 1),fromInteger (floor y)) `elem` terras && (fromInteger (floor x - 1),fromInteger (floor y)) `elem` posicoesPosiveis then Oeste
             else if (fromInteger (floor x),fromInteger (floor y - 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y - 1)) `elem` posicoesPosiveis then Norte
             else if (fromInteger (floor x),fromInteger (floor y + 1)) `elem` terras && (fromInteger (floor x),fromInteger (floor y + 1)) `elem` posicoesPosiveis then Sul
             else Este
    | otherwise = direcao
        where posicoesPosiveis = posicoesPossiveisParaPortais (filter (\(a,b) -> (a,b) /= (fromIntegral (floor x),fromIntegral (floor y))) terras) (fromIntegral (floor xb),fromIntegral (floor yb))

-- | A função 'atualizaInimigo' atualiza a posição, a vida e a direção de um inimigo com base nas suas circuntâncias (projéteis, posição no mapa, entre outros fatores).
--
-- == Exemplos
--
-- >>> atualizaInimigo 1 0 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Fogo (Finita 3)] [] 100 1)
-- Inimigo (1.5,0.5) Este 90 1 10 10 [Projetil Fogo (Finita 2)] [] 100 1
--
-- >>> atualizaInimigo 1 0 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Gelo (Finita 3)] [] 100 1)
-- Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Gelo (Finita 2)] [] 100 1
--
-- >>> atualizaInimigo 1 0 mapa01 (5,2) False (Inimigo (0.5,0.5) Norte 100 1 10 10 [Projetil Resina Infinita] [] 100 1)
-- Inimigo (1.2,0.5) Este 100 1 10 10 [Projetil Resina Infinita] [] 100 1

atualizaInimigo ::
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo do dia.
 Tempo ->
 -- | Mapa do jogo.
 Mapa ->
 -- | Posição da base.
 Posicao ->
 -- | Se está ou não a chover.
 Bool ->
 -- | Inimigo a atualizar.
 Inimigo ->
 -- | Inimigo atualizado.
 Inimigo
atualizaInimigo t td m posBase b (Inimigo (x,y) d vida velocidade dano butim projeteis imunidades vidMax num) = case projeteis of
    [] -> Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (velocidade * efeitoChuva * efeitoNoite)) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) vida velocidade dano butim [] imunidades vidMax num
        where efeitoChuva = if b then 0.85 else 1
              efeitoNoite = if num == 9 && (td <= 3 || td >= 21) then 1.2 else 1
    [Projetil Fogo duracao] -> Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (velocidade * efeitoChuva * efeitoNoite)) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (vida - 10 * t) velocidade dano butim (atualizaProjetil t [Projetil Fogo duracao]) imunidades vidMax num
        where efeitoChuva = if b then 0.85 else 1
              efeitoNoite = if num == 9 && (td <= 3 || td >= 21) then 1.2 else 1
    [Projetil Gelo duracao] -> Inimigo (x,y) d vida velocidade dano butim (atualizaProjetil t [Projetil Gelo duracao]) imunidades vidMax num
    [Projetil Resina duracao] -> Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (max 0 (velocidade * 0.7 * efeitoChuva * efeitoNoite))) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) vida velocidade dano butim (atualizaProjetil t [Projetil Resina duracao]) imunidades vidMax num
        where efeitoChuva = if b then 0.85 else 1
              efeitoNoite = if num == 9 && (td <= 3 || td >= 21) then 1.2 else 1
    projeteis' -> if any verifGelo projeteis' then Inimigo (x,y) d vida velocidade dano butim (atualizaProjetil t projeteis') imunidades vidMax num
                      else if any verifResina projeteis' then Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (max 0 (velocidade * 0.7 * efeitoChuva * efeitoNoite))) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) vida velocidade dano butim (atualizaProjetil t projeteis') imunidades vidMax num
                      else if any verifFogo projeteis' then Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (velocidade * efeitoChuva * efeitoNoite)) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (vida - 10 * t) velocidade dano butim (atualizaProjetil t projeteis') imunidades vidMax num
                      else Inimigo (atualizaPosicao (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) (x,y) t (velocidade * efeitoChuva * efeitoNoite)) (atualizaDirecao d (x,y) posBase (filtraPosicoesTerra m (0,0))) vida velocidade dano butim (atualizaProjetil t projeteis') imunidades vidMax num
                        where efeitoChuva = if b then 0.85 else 1
                              efeitoNoite = if num == 9 && (td <= 3 || td >= 21) then 1.2 else 1

-- | A função 'atualizaProjetil' atualiza a duração de um projétil (caso seja duração finita).
--
-- == Exemplos
--
-- >>> atualizaProjetil 1 [Projetil Fogo Infinita,Projetil Gelo (Finita 0.5)]
-- [Projetil Fogo Infinita]
--
-- >>> atualizaProjetil 1 [Projetil Fogo Infinita,Projetil Gelo (Finita 1.5)]
-- [Projetil Fogo Infinita,Projetil Gelo (Finita 0.5)]

atualizaProjetil ::
 -- | Tempo a atualizar.
 Tempo ->
 -- | Lista de projéteis a atualizar.
 [Projetil] ->
 -- | Lista de projéteis atualizados.
 [Projetil]
atualizaProjetil _ [] = []
atualizaProjetil t ((Projetil tipo (Finita x)) : ps)
    | x - t <= 0 = atualizaProjetil t ps
    | otherwise = Projetil tipo (Finita (x - t)) : atualizaProjetil t ps
atualizaProjetil t ((Projetil tipo Infinita) : ps) = Projetil tipo Infinita : atualizaProjetil t ps

-- | A função 'atualizaPortal' atualiza um portal de modo a que coloque em jogo inimigos das suas ondas quando estiver pronto.
--
-- == Exemplos
--
-- >>> atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) 5]) 1 []
-- (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-6) 4],[])
--
-- >>> atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) 1 []
-- atualizaPortal (Portal (4,4) [Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-5) (-5)]) 1 []

atualizaPortal ::
 -- | Portal a atualizar.
 Portal ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Lista de inimigos ativos do jogo.
 [Inimigo] ->
 -- | Resultado com o portal atualizado e todos os inimigos ativos do jogo.
 (Portal,[Inimigo])
atualizaPortal (Portal pos os) t is
    | not (existOndaAtiva os) = (Portal pos (map (atualizaTemposOnda t) os),is)
    | otherwise = let posOA = posicaoOndaAtiva os
                      (o',is') = atualizaOndaAtiva (os !! posOA) t pos
                      in (Portal pos (o' : map (atualizaTemposOnda t) (take posOA os) ++ map (atualizaTemposOnda t) (drop (posOA + 1) os)),is ++ is')

-- | A função 'mudaPosicaoInimigo' muda a posição de um inimigo.
--
-- == Exemplo
--
-- >>> mudaPosicaoInimigo (Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1) (4,4)
-- Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1

mudaPosicaoInimigo :: Inimigo -> Posicao -> Inimigo
mudaPosicaoInimigo (Inimigo _ direcao vida velocidade ataque butim projeteis imunidades vidaMax num) posicao = Inimigo posicao direcao vida velocidade ataque butim projeteis imunidades vidaMax num

-- | A função 'atualizaOndaAtiva' atualiza uma onda ativa, colocando em jogo um inimigo se estiver pronta.
--
-- == Exemplos
--
-- >>> atualizaOndaAtiva (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 5 (-5)) 1 (4,4)
-- (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 4 (-5),[])
--
-- >>> atualizaOndaAtiva (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 (-4) (-5)) 1 (4,4)
-- (Onda [] 5 5 (-5),[Inimigo (4,4) Norte 40 40 40 40 [] [] 1 1])

atualizaOndaAtiva ::
 -- | Onda (ativa) a atualizar.
 Onda ->
 -- | Tempo a atualizar.
 Tempo -> 
 -- | Posição do portal.
 Posicao ->
 -- | Resultado com a onda atualizada e a lista de inimigos resultante.
 (Onda,[Inimigo])
atualizaOndaAtiva (Onda (i:is) ciclo tempo entrada) t (x,y)
    | tempo <= 0 = (Onda is ciclo ciclo entrada,[mudaPosicaoInimigo i (x,y)])
    | tempo >= 0 = (Onda (i:is) ciclo (tempo - t) entrada,[])

-- | A função 'atualizaTemposOnda' atualiza os tempos de uma onda (não ativa).
--
-- == Exemplos
--
-- >>> atualizaTemposOnda 1 (Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 5 100)
-- Onda [Inimigo (1,1) Norte 40 40 40 40 [] [] 1 1] 5 4 99

atualizaTemposOnda ::
 -- | Tempo a atualizar.
 Tempo ->
 -- | Onda (não ativa) a atualizar.
 Onda ->
 -- | Onda atualizada.
 Onda
atualizaTemposOnda t o = Onda is ciclo (tempo - t) (entrada - t)
    where Onda is ciclo tempo entrada = o

-- | A função 'atualizaVidaInimigosLagrimasCristalinas' aumenta a vida dos inimigos (ativos) próximos de lágrimas cristalinas.
--
-- == Exemplos
--
-- >>> atualizaVidaInimigosLagrimasCristalinas [(1,1)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (True,30,[])
-- [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1]
--
-- >>> atualizaVidaInimigosLagrimasCristalinas [(1,1)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (False,30,[])
-- [Inimigo (1,1) Norte 45 40 40 40 [] [] 45 1]
--
-- >>> atualizaVidaInimigosLagrimasCristalinas [(5,5)] [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1] 1 (True,30,[])
-- [Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1]

atualizaVidaInimigosLagrimasCristalinas ::
 -- | Posições das águas cristalinas.
 [Posicao] ->
 -- | Inimigos ativos do jogo.
 [Inimigo] ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo de jogo.
 (Bool,Tempo,[Posicao]) ->
 -- | Inimigos eventualmente regenerados.
 [Inimigo]
atualizaVidaInimigosLagrimasCristalinas lcs is t ec
    | (\(a,_,_) -> a) ec = is
    | otherwise = map (atualizaVidaInimigoLagrimasCristalinas lcs t) is

-- | A função 'atualizaVidaInimigoLagrimasCristalinas' aumenta a vida de um inimigo caso este esteja próximo de lágrimas cristalinas.
--
-- == Exemplos
--
-- >>> atualizaVidaInimigoLagrimasCristalinas [(1,1)] 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1)
-- Inimigo (1,1) Norte 45 40 40 40 [] [] 45 1
--
-- >>> atualizaVidaInimigoLagrimasCristalinas [(5,5)] 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1

atualizaVidaInimigoLagrimasCristalinas ::
 -- | Posicões das águas cristalinas.
 [Posicao] ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Inimigo selecionado.
 Inimigo ->
 -- | Inimigo eventualmente regenerado.
 Inimigo
atualizaVidaInimigoLagrimasCristalinas lcs t (Inimigo pos direcao vida velocidade ataque butim projeteis imunidades vidMax num)
    | null (filter (\(x,y) -> dist (x,y) pos <= 2.5) lcs) = Inimigo pos direcao vida velocidade ataque butim projeteis imunidades vidMax num
    | otherwise = Inimigo pos direcao (min (vida + 10 * t) vidMax) velocidade ataque butim projeteis imunidades vidMax num

-- | A função 'removeGotas' remove de uma lista de gotas de chuva aquelas que se situam fora do mapa (em relação às ordenadas).
--
-- == Exemplo
--
-- >>> removeGotas [(0,-900),(0,-200)]
-- [(0,-200)]

removeGotas :: 
 -- | Posições das gotas.
 [Posicao] -> 
 -- | Lista resultante de remover as gotas que já estão demasiado baixas.
 [Posicao]
removeGotas = filter (\(_,y) -> y > -896)

-- | A função 'atualizaPosicoesGotas' atualiza as posições das gotas.
--
-- == Exemplo
--
-- >>> atualizaPosicoesGotas [(100,-300),(200,-400)] 1
-- [(95,-400),(195,-500)]

atualizaPosicoesGotas :: 
 -- | Posições das gotas.
 [Posicao] -> 
 -- | Tempo a atualizar.
 Tempo ->
 -- | Posições atualizadas.
 [Posicao]
atualizaPosicoesGotas [] _ = []
atualizaPosicoesGotas ((x,y):ps) t = (x - 5 * t,y - 100 * t) : atualizaPosicoesGotas ps t

-- | A função 'removeProjetilFogo' remove os projéteis de fogo de um inimigo.
--
-- == Exemplo
--
-- >>> removeProjetilFogo (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1

removeProjetilFogo :: 
 -- | Inimigo de escolha.
 Inimigo -> 
 -- | Inimigo sem projéteis de fogo.
 Inimigo
removeProjetilFogo (Inimigo pos dir vida velocidade ataque butim ps imunidades vMax n) = Inimigo pos dir vida velocidade ataque butim ps' imunidades vMax n
    where ps' = filter (\p -> tipoProjetil p /= Fogo) ps

-- | A função 'removeProjetilGelo' remove os projéteis de gelo de um inimigo.
--
-- == Exemplo
--
-- >>> removeProjetilGelo (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1

removeProjetilGelo :: 
 -- | Inimigo de escolha.
 Inimigo -> 
 -- | Inimigo sem projéteis de fogo.
 Inimigo
removeProjetilGelo (Inimigo pos dir vida velocidade ataque butim ps imunidades vMax n) = Inimigo pos dir vida velocidade ataque butim ps' imunidades vMax n
    where ps' = filter (\p -> tipoProjetil p /= Gelo) ps


-- | A função 'adicionaProjetilFogo' adiciona um projétil de fogo a um inimigo.
--
-- == Exemplos
--
-- >>> adicionaProjetilFogo 1 (Inimigo (1,1) Norte 40 40 40 40 [] [] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo (Finita 1)] [] 45 1
--
-- >>> adicionaProjetilFogo 1 (Inimigo (1,1) Norte 40 40 40 40 [] [Fogo] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [] [Fogo] 45 1

adicionaProjetilFogo :: 
 -- | Tempo a atualizar.
 Tempo ->
 -- | Inimigo de escolha.
 Inimigo -> 
 -- | Inimigo com projétil do fogo adicionado.
 Inimigo
adicionaProjetilFogo t (Inimigo pos dir vida velocidade ataque butim ps imunidades vMax n) = Inimigo pos dir vida velocidade ataque butim ps' imunidades vMax n
    where ps' = if Fogo `elem` imunidades then ps else normalizaProjeteis (Projetil Fogo (Finita t) : ps)

-- | A função 'removeProjetilResina' remove os projéteis de resina de um inimigo.
--
-- == Exemplo
--
-- >>> removeProjetilResina (Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Resina Infinita,Projetil Fraqueza Infinita] [] 45 1)
-- Inimigo (1,1) Norte 40 40 40 40 [Projetil Fogo Infinita,Projetil Gelo Infinita,Projetil Fraqueza Infinita] [] 45 1

removeProjetilResina :: 
 -- | Inimigo de escolha.
 Inimigo -> 
 -- | Inimigo sem projéteis de resina.
 Inimigo
removeProjetilResina (Inimigo pos dir vida velocidade ataque butim ps imunidades vMax n) = Inimigo pos dir vida velocidade ataque butim ps' imunidades vMax n
    where ps' = filter (\p -> tipoProjetil p /= Resina) ps





-- {- Funções Principais -} --

-- | A função 'atualizaTorres' permite que as torres realizem devidamente os seus ataques aos inimigos ativos do jogo.
--
-- == Exemplo
--
-- >>> atualizaTorres [Torre (1,1) 5 1 1 5 0 (Projetil Fogo Infinita) 1 1,Torre (4,4) 5 1 1 5 1 (Projetil Fogo Infinita) 1 1] [Inimigo (1,1) Norte 40 40 40 40 [] [] 40 1,Inimigo (4,4) Norte 40 40 40 40 [] [] 40 1] 1 1
-- ([Torre (1,1) 5 1 1 5 5 (Projetil Fogo Infinita) 1 1,Torre (4,4) 5 1 1 5 0 (Projetil Fogo Infinita) 1 1],[Inimigo (4,4) Norte 40 40 40 40 [] [] 40 1,Inimigo (1,1) Norte 35 40 40 40 [Projetil Fogo Infinita] [] 40 1])

atualizaTorres ::
 -- | Lista de torres do jogo.
 [Torre] ->
 -- | Lista de inimigos ativos do jogo.
 [Inimigo] ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo do dia.
 Tempo ->
 -- | Tempo do jogo.
 Tempo ->
 -- | Resultado com as torres e os inimigos ativos do jogo atualizados.
 ([Torre],[Inimigo])
atualizaTorres [] is _ _ _ = ([],is)
atualizaTorres (torre:ts) is t td tj = (torreAtualizada : ts',is')
    where (torreAtualizada,isAtualizados) = detetaInimigos torre (inimigosNoAlcance torre is) t td tj
          (ts',is') = atualizaTorres ts (isAtualizados ++ filter (inimigoNaoEstaNoAlcance torre) is) t td tj

-- | A função 'atualizaInimigos' atualiza os inimigos do jogo ao remover os mortos e os que já alcançaram a base (assim como as consequências destes acontecimentos) e ao atualizar
-- o movimento dos restantes.
--
-- == Exemplo
--
-- >>> atualizaInimigos [Inimigo (0.5,0.5) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1,Inimigo (0.5,0.5) Norte 40 1 40 40 [] [] 40 1] mapa01 (Base 100 (1,4) 100) 1 0 5 False
-- (Base 100 (1,4) 100,[Inimigo (1.5,0.5) Este (-5) 1 40 40 [Projetil Fogo Infinita] [] 40 1,Inimigo (1.5,0.5) Este 40 1 40 40 [] [] 40 1],5)

atualizaInimigos ::
 -- | Inimigos ativos do jogo.
 [Inimigo] ->
 -- | Mapa do jogo.
 Mapa ->
 -- | Base do jogo.
 Base ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo do dia.
 Tempo ->
 -- | Quantidade de inimigos derrotados.
 Int ->
 -- | Se a chuva está ou não ativa.
 Bool ->
 -- | Resultado com a base e os inimigos atualizados.
 (Base,[Inimigo],Int)
atualizaInimigos inimigos mapa base t td q b = (baseAtualizada',map (atualizaInimigo t td mapa (posicaoBase base) b) inimigosAtualizados,q')
    where (baseAtualizada,inimigosVivos,q') = removeInimigosMortos inimigos base q
          (baseAtualizada',inimigosAtualizados) = inimigosAtingemBase baseAtualizada inimigosVivos

-- | A função 'atualizaPortais' atualiza os portais de modo a que coloquem em jogo inimigos das suas ondas quando estiverem prontos.
--
-- == Exemplo
--
-- >>> atualizaPortais [Portal (4,4) [Onda [Inimigo (0.5,0.5) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1] 5 0 (-5)]] 1 []
-- ([Portal (4,4) [Onda [] 5 5 (-5)]],[Inimigo (4,4) Norte 5 1 40 40 [Projetil Fogo Infinita] [] 40 1])

atualizaPortais ::
 -- | Lista de portais de jogo.
 [Portal] ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Lista de inimigos ativos do jogo.
 [Inimigo] ->
 -- | Resultado com os portais atualizados e todos os inimigos ativos do jogo.
 ([Portal],[Inimigo])
atualizaPortais [] _ is = ([],is)
atualizaPortais (p:ps) t is = (p' : ps',is'')
    where (p',is') = atualizaPortal p t is
          (ps',is'') = atualizaPortais ps t is'

-- | A função 'atualizaGotasChuva' atualiza a chuva no jogo.

atualizaGotasChuva :: 
 -- | Estado inicial da chuva.
 (Bool,Tempo,[Posicao]) -> 
 -- | Tempo de jogo (multiplicado por 60).
 Tempo -> 
 -- | Tempo a atualizar.
 Tempo ->
 -- | Estado atualizado da chuva.
 (Bool,Tempo,[Posicao])
atualizaGotasChuva (False,tc,ps) tj t
    | tc > 30 && mod (head (geraAleatorios (floor tj) 1)) 1800 == 0 = (True,0,ps)
    | otherwise = (False,tc + t,atualizaPosicoesGotas ps' t)
        where ps' = removeGotas ps
atualizaGotasChuva (True,tc,ps) tj t
    | tc > 15 = (False,0,atualizaPosicoesGotas ps' t)
    | null (filter (\(_,y) -> y > -128) ps) = let [x1,x2,x3,x4,x5,x6,x7,x8] = geraAleatorios (floor tj) 8 in 
        (True,tc + t,[(fromIntegral (mod x1 1344),0),(fromIntegral (mod x2 1344),0),(fromIntegral (mod x3 1344),0),(fromIntegral (mod x4 1344),0),
                      (fromIntegral (mod x5 1344),0),(fromIntegral (mod x6 1344),0),(fromIntegral (mod x7 1344),0),(fromIntegral (mod x8 1344),0)] ++ atualizaPosicoesGotas ps' t)
    | otherwise = (True,tc + t,atualizaPosicoesGotas ps' t)
            where ps' = removeGotas ps

-- | A função 'atualizaTempoDia' atualiza o tempo do dia do jogo.

atualizaTempoDia ::
 -- | Tempo do dia pré-atualização.
 Tempo ->
 -- | Tempo a atualizar.
 Tempo ->
 -- | Tempo do dia atualizado.
 Tempo
atualizaTempoDia td t
    | td >= 24 = 0
    | otherwise = td + t / 8