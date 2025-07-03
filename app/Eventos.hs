{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Eventos where

import LI12425
import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers

-- {- Funções Auxiliares -} --

-- | A função 'dist' determina a distância entre dois pontos de um plano.

dist :: 
 -- | Posição do primeiro ponto.
 Posicao -> 
 -- | Posição do segundo ponto.
 Posicao ->
 -- | Distância entre os dois pontos.
 Float
dist (x1,y1) (x2,y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

-- | A função 'mudaPosicaoTorre' altera a posição de uma torre para outra dada.

mudaPosicaoTorre ::
 -- | Torre pré-mudança.
 Torre ->
 -- | Posição nova.
 Posicao ->
 -- | Torre pós-mudança
 Torre
mudaPosicaoTorre (Torre _ dano alcance rajada ciclo tempo projetil prob1 prob2 nivel numero) pos = Torre pos dano alcance rajada ciclo tempo projetil prob1 prob2 nivel numero

-- | A função 'nivelAtual' recebe o número do nível e devolve o próprio nível.

nivelAtual ::
 -- | Número do nível.
 Int ->
 -- | Jogo correspondente ao nível.
 Jogo
nivelAtual n
    | n == 1 = planiciesVerdejantes
    | n == 5 = planiciesVerdejantesElite
    | n == 2 = florestaDaMorte
    | n == 6 = florestaDaMorteElite
    | n == 3 = vulcaoDasTrevas
    | n == 7 = vulcaoDasTrevasElite
    | n == 4 = ilhasCristalinas
    | n == 8 = ilhasCristalinasElite

-- | A função 'mudaDificuldadeNivel' recebe o número do nível e devolve o próprio nível com a dificuldade alterada.
mudaDificuldadeNivel ::
 -- | Número do nível.
 Int ->
 -- | Jogo correspondente ao nível com a dificulade alterada.
 Jogo
mudaDificuldadeNivel n
    | n == 1 = planiciesVerdejantesElite
    | n == 2 = florestaDaMorteElite
    | n == 3 = vulcaoDasTrevasElite
    | n == 4 = ilhasCristalinasElite
    | n == 5 = planiciesVerdejantes
    | n == 6 = florestaDaMorte
    | n == 7 = vulcaoDasTrevas
    | n == 8 = ilhasCristalinas

-- | A função 'nivelSeguinte' recebe o número do nível e devolve o nível seguinte.

nivelSeguinte ::
 -- | Número do nível.
 Int ->
 -- | Jogo correspondente ao nível seguinte.
 Jogo
nivelSeguinte n
    | n == 1 = florestaDaMorte
    | n == 5 = florestaDaMorte
    | n == 2 = vulcaoDasTrevas
    | n == 6 = vulcaoDasTrevas

-- | A função 'torreNaPosicao' determina a torre que está numa dada posição no mapa, caso esta exista.

torreNaPosicao ::
 -- | Posição dada.
 Posicao ->
 -- | Lista de torres do jogo.
 [Torre] ->
 -- | Resultado na forma [] caso não exista nenhuma torre na dada posição ou [torre] caso exista.
 [Torre]
torreNaPosicao _ [] = []
torreNaPosicao posicao (t:ts)
    | posicaoTorre t == posicao = [t]
    | otherwise = torreNaPosicao posicao ts

-- | A função 'determinaCustoVendaTorre' determina o valor de venda de uma torre.

determinaCustoVendaTorre :: 
 -- | Torre selecionada.
 Torre ->
 -- | Loja do jogo.
 Loja ->
 -- | Valor de venda da torre.
 Creditos
determinaCustoVendaTorre torre loja = floor (fromIntegral (fst (loja !! (n - 1))) * fromIntegral m * 0.6)
        where n = numeroTorre torre
              m = nivelTorre torre

-- | A função 'determinaCustoVendaTorre' determina o valor de melhora de uma torre.

determinaCustoMelhoraTorre :: 
 -- | Torre selecionada.
 Torre ->
 -- | Loja do jogo.
 Loja ->
 -- | Valor de melhora da torre.
 Creditos
determinaCustoMelhoraTorre torre loja = fst (loja !! (n - 1))
        where n = numeroTorre torre

-- | A função 'torreNivelSeguinte' devolve, dada uma torre de nível 1 ou 2, a torre do nível seguinte assim como o custo de melhora.

torreNivelSeguinte ::
 -- | Torre selecionada.
 Torre ->
 -- | Posição nova da torre.
 Posicao ->
 -- | Resultado na forma (custo,torreNova).
 (Creditos,Torre)
torreNivelSeguinte torre (x,y)
    | nivel == 1 && n == 1 = (fst arqueiroFlamejante,mudaPosicaoTorre arqueiroFlamejante2 (x,y))
    | nivel == 1 && n == 2 = (fst sabioCorruptor,mudaPosicaoTorre sabioCorruptor2 (x,y))
    | nivel == 1 && n == 3 = (fst carvalhoEnt,mudaPosicaoTorre carvalhoEnt2 (x,y))
    | nivel == 1 && n == 4 = (fst maquinaDeResina,mudaPosicaoTorre maquinaDeResina2 (x,y))
    | nivel == 1 && n == 5 = (fst magoDeGelo,mudaPosicaoTorre magoDeGelo2 (x,y))
    | nivel == 1 && n == 6 = (fst fornalhaArdente,mudaPosicaoTorre fornalhaArdente2 (x,y))
    | nivel == 1 && n == 7 = (fst canhao,mudaPosicaoTorre canhao2 (x,y))
    | nivel == 1 && n == 8 = (fst juizGelado,mudaPosicaoTorre juizGelado2 (x,y))
    | nivel == 2 && n == 1 = (fst arqueiroFlamejante,mudaPosicaoTorre arqueiroFlamejante3 (x,y))
    | nivel == 2 && n == 2 = (fst sabioCorruptor,mudaPosicaoTorre sabioCorruptor3 (x,y))
    | nivel == 2 && n == 3 = (fst carvalhoEnt,mudaPosicaoTorre carvalhoEnt3 (x,y))
    | nivel == 2 && n == 4 = (fst maquinaDeResina,mudaPosicaoTorre maquinaDeResina3 (x,y))
    | nivel == 2 && n == 5 = (fst magoDeGelo,mudaPosicaoTorre magoDeGelo3 (x,y))
    | nivel == 2 && n == 6 = (fst fornalhaArdente,mudaPosicaoTorre fornalhaArdente3 (x,y))
    | nivel == 2 && n == 7 = (fst canhao,mudaPosicaoTorre canhao3 (x,y))
    | nivel == 2 && n == 8 = (fst juizGelado,mudaPosicaoTorre juizGelado3 (x,y))
        where nivel = nivelTorre torre
              n = numeroTorre torre

-- | A função 'muda' muda o '1' para o '2' e o '2' para o '1'.

muda :: Int -> Int
muda 1 = 2
muda 2 = 1

-- | A função 'geraTempoDia' gera um tempo do dia aleatório.

geraTempoDia :: Jogo -> Tempo -> Jogo
geraTempoDia jogo tj = Jogo base portais torres mapa inimigos loja n p isd ec (fromIntegral (mod (head (geraAleatorios (floor (tj * 60)) 1)) 24))
    where Jogo base portais torres mapa inimigos loja n p isd ec _ = jogo





-- {- Função Principal -} --

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (MenuPrincipal q cs nc t b tj)
    | x >= -448 && x <= -64 && y >= 84 && y <= 212 = SelecaoNiveis q cs nc t b tj
    | x >= 64 && x <= 448 && y >= 84 && y <= 212 = Conquistas q cs nc t b tj
    | x >= -448 && x <= -64 && y >= -84 && y <= 44 = InfoTorres q cs nc t b Nothing tj
    | x >= 64 && x <= 448 && y >= -84 && y <= 44 = InfoInimigos q cs nc t b Nothing 1 tj
    | x >= -448 && x <= -64 && y >= -252 && y <= -124 = Temas q cs nc t b tj
    | dist (x,y) (-850,-430) <= 53.3 = InfoJogo q cs nc t b tj 1
    | x >= 64 && x <= 448 && y >= -252 && y <= -124 = undefined
    | otherwise = MenuPrincipal q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Temas q cs nc t b tj)
    | x >= -191 && x <= 192 && y >= 344 && y <= 472 = MenuPrincipal q cs nc t b tj
    | 3 `elem` nc && x >= -191 && x <= 192 && y >= -473 && y <= -345 = Temas q cs nc Flamejante b tj
    | x >= -831 && x <= -447 && y >= -473 && y <= -345 = Temas q cs nc Natural b tj
    | 4 `elem` nc && x >= 448 && x <= 832 && y >= -473 && y <= -345 = Temas q cs nc Cristalino b tj
    | otherwise = Temas q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (InfoJogo q cs nc t b tj n)
    | x >= -191 && x <= 192 && y >= 344 && y <= 472 = MenuPrincipal q cs nc t b tj
    | x >= -191 && x <= 192 && y >= -308 && y <= -180 = InfoJogo q cs nc t b tj nMais
    | x >= -192 && x <= 192 && y >= -476 && y <= -348 = InfoJogo q cs nc t b tj nMenos
    | otherwise = InfoJogo q cs nc t b tj n
        where nMais = if n == 12 then 1 else n + 1
              nMenos = if n == 1 then 12 else n - 1
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (InfoJogo q cs nc t b tj n) = InfoJogo q cs nc t b tj nMais
    where nMais = if n == 12 then 1 else n + 1
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (InfoJogo q cs nc t b tj n) = InfoJogo q cs nc t b tj nMenos
    where nMenos = if n == 1 then 12 else n - 1
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (InfoTorres q cs nc t b jogo tj) = case jogo of
    Nothing -> if x >= -191 && x <= 192 && y >= -64 && y <= 64 then MenuPrincipal q cs nc t b tj else InfoTorres q cs nc t b Nothing tj
    Just j -> if x >= -191 && x <= 192 && y >= -64 && y <= 64 then JogoPausado j q cs nc t b tj else InfoTorres q cs nc t b (Just j) tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (InfoInimigos q cs nc t b jogo n tj) = case jogo of
    Nothing -> if x >= -191 && x <= 192 && y >= -18 && y <= 146 then MenuPrincipal q cs nc t b tj
               else if x >= -191 && x <= 192 && y >= -144 && y <= -15 then InfoInimigos q cs nc t b jogo (muda n) tj 
               else InfoInimigos q cs nc t b jogo n tj
    Just j -> if x >= -191 && x <= 192 && y >= -18 && y <= 146 then JogoPausado j q cs nc t b tj
              else if x >= -191 && x <= 192 && y >= -144 && y <= -15 then InfoInimigos q cs nc t b jogo (muda n) tj
              else InfoInimigos q cs nc t b jogo n tj
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (InfoInimigos q cs nc t b jogo n tj) = InfoInimigos q cs nc t b jogo (muda n) tj
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (InfoInimigos q cs nc t b jogo n tj) = InfoInimigos q cs nc t b jogo (muda n) tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (Conquistas q cs nc t b tj)
    | x >= -191 && x <= 192 && y >= -64 && y <= 64 = MenuPrincipal q cs nc t b tj
    | otherwise = Conquistas q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (JogoPausado jogo q cs nc t b tj)
    | x >= -448 && x <= -64 && y >= 84 && y <= 212 = JogoACorrer jogo q cs nc t b tj
    | x >= 64 && x <= 448 && y >= 84 && y <= 212 = JogoACorrer (nivelAtual (numeroNivel jogo)) (q + inimigosDerrotados jogo) cs nc t False tj
    | x >= -448 && x <= -64 && y >= -84 && y <= 44 = InfoTorres q cs nc t b (Just jogo) tj
    | x >= 64 && x <= 448 && y >= -84 && y <= 44 = InfoInimigos q cs nc t b (Just jogo) 1 tj
    | x >= -448 && x <= -64 && y >= -252 && y <= -124 = SelecaoNiveis (q + inimigosDerrotados jogo) cs nc t False tj
    | x >= 64 && x <= 448 && y >= -252 && y <= -124 = MenuPrincipal (q + inimigosDerrotados jogo) cs nc t False tj
    | otherwise = JogoPausado jogo q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (PerdeuJogo jogo q cs nc t b tj)
    | x >= -191 && x <= 192 && y >= -159 && y <= -31 = SelecaoNiveis q cs nc t b tj
    | x >= -191 && x <= 192 && y >= 32 && y <= 160 = JogoACorrer (nivelAtual (numeroNivel jogo)) q cs nc t b tj
    | otherwise = PerdeuJogo jogo q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (GanhouJogo jogo q cs nc t b tj)
    | x >= -191 && x <= 192 && y >= -284 && y <= -156 = SelecaoNiveis q cs nc t b tj
    | x >= -191 && x <= 192 && y >= -452 && y <= -324 = JogoACorrer (mudaDificuldadeNivel (numeroNivel jogo)) q cs nc t b tj
    | numeroNivel jogo /= 3 && numeroNivel jogo /= 7 && numeroNivel jogo /= 4 && numeroNivel jogo /= 8 && x >= -191 && x <= 192 && y >= 52 && y <= 180 = JogoACorrer (nivelSeguinte (numeroNivel jogo)) q cs nc t b tj
    | x >= -191 && x <= 192 && y >= -116 && y <= 12 = JogoACorrer (nivelAtual (numeroNivel jogo)) q cs nc t b tj
    | otherwise = GanhouJogo jogo q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (SelecaoNiveis q cs nc t b tj)
    | x >= -239 && x <= 240 && y >= -79 && y <= 80 = MenuPrincipal q cs nc t b tj
    | x >= -743 && x <= -263 && y >= 120 && y <= 280 = JogoACorrer (geraTempoDia planiciesVerdejantes tj) q cs nc t b tj
    | 1 `elem` nc && x >= 266 && x <= 746 && y >= 120 && y <= 280 = JogoACorrer (geraTempoDia planiciesVerdejantesElite tj) q cs nc t b tj
    | 1 `elem` nc && x >= -805 && x <= -325 && y >= -79 && y <= 80 = JogoACorrer (geraTempoDia florestaDaMorte tj) q cs nc t b tj
    | 2 `elem` nc && x >= 328 && x <= 808 && y >= -79 && y <= 80 = JogoACorrer (geraTempoDia florestaDaMorteElite tj) q cs nc t b tj
    | 2 `elem` nc && x >= -743 && x <= -263 && y >= -279 && y <= -119 = JogoACorrer (geraTempoDia vulcaoDasTrevas tj) q cs nc t b tj
    | 3 `elem` nc && x >= 266 && x <= 746 && y >= -279 && y <= -119 = JogoACorrer (geraTempoDia vulcaoDasTrevasElite tj) q cs nc t b tj
    | x >= -805 && x <= -325 && y >= -479 && y <= -319 && length cs >= 5 && q >= 1000 = JogoACorrer (geraTempoDia ilhasCristalinas tj) q cs nc t b tj
    | x >= -328 && x <= 808 && y >= -479 && y <= -319 && length cs >= 5 && q >= 1000 && 4 `elem` nc = JogoACorrer (geraTempoDia ilhasCristalinasElite tj) q cs nc t b tj
    | otherwise = SelecaoNiveis q cs nc t b tj
reageEventos (EventKey (MouseButton LeftButton) Down _ (x,y)) (JogoACorrer jogo q cs nc t b tj)
    | x >= 429 && x <= 927 && y >= 404 && y <= 505 = JogoPausado jogo q cs nc t b tj
    | div (floor (x + 926)) 64 >= 0 && div (floor (x + 926)) 64 <= xMax && div (floor (-y) + 391) 64 >= 0 && div (floor (-y) + 391) 64 <= yMax = JogoACorrer (Jogo base portais torres mapa inimigos loja numero (fromIntegral (div (floor x + 926) 64) + 0.5,fromIntegral (div (floor (-y) + 391) 64) + 0.5) q' gc td) q cs nc t b tj
    | otherwise = JogoACorrer jogo q cs nc t b tj
        where Jogo base portais torres mapa inimigos loja numero _ q' gc td = jogo
              xMax = length (head mapa) - 1
              yMax = length mapa - 1
reageEventos (EventKey (Char 'a') Down _ _) (JogoACorrer jogo q cs nc t b tj) = JogoACorrer jogo q cs nc t (not b) tj
reageEventos (EventKey (SpecialKey KeyUp) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | floor y < 1 = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo base portais torres mapa inimigos loja numero (x,y - 1) q' gc td) q cs nc t b tj
        where Jogo base portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
reageEventos (EventKey (SpecialKey KeyDown) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | floor y + 1 > yMax = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo base portais torres mapa inimigos loja numero (x,y + 1) q' gc td) q cs nc t b tj
        where Jogo base portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              yMax = length mapa - 1
reageEventos (EventKey (SpecialKey KeyRight) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | floor x + 1 > xMax = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo base portais torres mapa inimigos loja numero (x + 1,y) q' gc td) q cs nc t b tj
        where Jogo base portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              xMax = length (head mapa) - 1
reageEventos (EventKey (SpecialKey KeyLeft) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | floor x < 1 = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo base portais torres mapa inimigos loja numero (x - 1,y) q' gc td) q cs nc t b tj
        where Jogo base portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
reageEventos (EventKey (Char 'r') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | null (torreNaPosicao (x,y) torres) = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vida posicao (creditos + determinaCustoVendaTorre (head (torreNaPosicao (x,y) torres)) loja)) portais (filter (\k -> posicaoTorre k /= (x,y)) torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vida posicao creditos) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | null (torreNaPosicao (x,y) torres) = JogoACorrer jogo q cs nc t b tj 
    | creditos < determinaCustoMelhoraTorre (head (torreNaPosicao (x,y) torres)) loja || nivelTorre (head (torreNaPosicao (x,y) torres)) == 3 = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vida posicao (creditos - determinaCustoMelhoraTorre (head (torreNaPosicao (x,y) torres)) loja)) portais (snd (torreNivelSeguinte (head (torreNaPosicao (x,y) torres)) (x,y)) : filter (\k -> posicaoTorre k /= (x,y))torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vida posicao creditos) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
reageEventos (EventKey (MouseButton RightButton) Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | null (torreNaPosicao (x,y) torres) = JogoACorrer jogo q cs nc t b tj 
    | creditos < determinaCustoMelhoraTorre (head (torreNaPosicao (x,y) torres)) loja || nivelTorre (head (torreNaPosicao (x,y) torres)) == 3 = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vida posicao (creditos - determinaCustoMelhoraTorre (head (torreNaPosicao (x,y) torres)) loja)) portais (snd (torreNivelSeguinte (head (torreNaPosicao (x,y) torres)) (x,y)) : filter (\k -> posicaoTorre k /= (x,y))torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vida posicao creditos) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
reageEventos (EventKey (Char '1') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | null loja = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd arqueiroFlamejante) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if null loja then 0 else fst (head loja)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '2') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 2 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd sabioCorruptor) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 2 then 0 else fst (loja !! 1)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '3') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 3 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd carvalhoEnt) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 3 then 0 else fst (loja !! 2)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '4') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 4 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd maquinaDeResina) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 4 then 0 else fst (loja !! 3)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '5') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 5 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd magoDeGelo) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 5 then 0 else fst (loja !! 4)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '6') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 6 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd fornalhaArdente) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 6 then 0 else fst (loja !! 5)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '7') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 7 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd canhao) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 7 then 0 else fst (loja !! 6)
              posicoesTorres = map posicaoTorre torres
reageEventos (EventKey (Char '8') Down _ _) (JogoACorrer jogo q cs nc t b tj)
    | length loja < 8 = JogoACorrer jogo q cs nc t b tj
    | (mapa !! floor y) !! floor x == Terra || (mapa !! floor y) !! floor x == Agua || (mapa !! floor y) !! floor x == Lava || (mapa !! floor y) !! floor x == LagrimasCristalinas = JogoACorrer jogo q cs nc t b tj
    | (x,y) `elem` posicoesTorres = JogoACorrer jogo q cs nc t b tj
    | credBase < custoTorre = JogoACorrer jogo q cs nc t b tj
    | otherwise = JogoACorrer (Jogo (Base vidBase posBase (credBase - custoTorre)) portais (mudaPosicaoTorre (snd juizGelado) (x,y) : torres) mapa inimigos loja numero (x,y) q' gc td) q cs nc t b tj
        where Jogo (Base vidBase posBase credBase) portais torres mapa inimigos loja numero (x,y) q' gc td = jogo
              custoTorre = if length loja < 8 then 0 else fst (loja !! 7)
              posicoesTorres = map posicaoTorre torres
reageEventos _ estado = estado