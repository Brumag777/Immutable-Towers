{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Tarefa2

-- {- Função Principal -} --

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it = case it of
    JogoACorrer jogo q cs nc tema b tj -> let nc' = if numeroNivel jogo `elem` nc then nc else nc ++ [numeroNivel jogo] 
                                              conq4 = if vidaBase (baseJogo jogo) /= 100 || 4 `elem` cs then [] else [4]
                                              conq5 = if vidaBase (baseJogo jogo) /= 100 || (numeroNivel jogo /= 3 && numeroNivel jogo /= 7) || 5 `elem` cs then [] else [5]
                                              conq6 = if 3 /= numeroNivel jogo || 6 `elem` cs then [] else [6]
                                              conq7 = if length (torresJogo jogo) < 20 || 7 `elem` cs then [] else [7]
                                              conq8 = if elementosDiferentes (map numeroTorre (torresJogo jogo)) < 6 || 8 `elem` cs then [] else [8] in
                                              if ganhouJogo jogo then GanhouJogo jogo (q + inimigosDerrotados jogo) (cs ++ conq4 ++ conq5 ++ conq6 ++ conq7 ++ conq8) nc' tema False (tj + t)
                                              else if perdeuJogo jogo then PerdeuJogo jogo (q + inimigosDerrotados jogo) cs nc tema False (tj + t)
                                              else JogoACorrer (atualizaJogo t tj jogo) q cs nc tema b (tj + t)
    MenuPrincipal q cs nc tema b tj -> MenuPrincipal q cs nc tema b (tj + t)
    Conquistas q cs nc tema b tj -> Conquistas q cs nc tema b (tj + t)
    SelecaoNiveis q cs nc tema b tj -> SelecaoNiveis q cs nc tema b (tj + t)
    JogoPausado jogo q cs nc tema b tj -> JogoPausado jogo q cs nc tema b (tj + t)
    PerdeuJogo jogo q cs nc tema b tj -> PerdeuJogo jogo q cs nc tema b (tj + t)
    GanhouJogo jogo q cs nc tema b tj -> GanhouJogo jogo q cs nc tema b (tj + t)
    Temas q cs nc tema b tj -> Temas q cs nc tema b (tj + t)
    InfoTorres q cs nc tema b jogo tj -> InfoTorres q cs nc tema b jogo (tj + t)
    InfoInimigos q cs nc tema b jogo n tj -> InfoInimigos q cs nc tema b jogo n (tj + t)
    InfoJogo q cs nc tema b tj n -> InfoJogo q cs nc tema b (tj + t) n
 




-- {- Função Auxiliar -} --

elementosDiferentes :: Eq a => [a] -> Int
elementosDiferentes [] = 0
elementosDiferentes (h:t) = 1 + elementosDiferentes (filter (/= h) t)