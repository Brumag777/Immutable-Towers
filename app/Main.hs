module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo

janela :: Display
janela = InWindow "Immutable Towers" (1920,1080) (0,0)

fundo :: Color
fundo = black

fr :: Int
fr = 60

main :: IO ()
main = do imagemBackgroundNatural <- loadBMP "imagens/backgroundnatural.bmp"
          imagemBackgroundFlamejante <- loadBMP "imagens/backgroundflamejante.bmp"
          imagemBackgroundCristalina <- loadBMP "imagens/backgroundcristalina.bmp"
          imagemBackgroundFlamejanteDiminuida <- loadBMP "imagens/backgroundflamejantediminuida.bmp"
          imagemBackgroundCristalinaDiminuida <- loadBMP "imagens/backgroundcristalinadiminuida.bmp"
          imagemLogoJogo <- loadBMP "imagens/logojogo.bmp"
          texturaRelva <- loadBMP "imagens/texturarelva.bmp"
          texturaTerra <- loadBMP "imagens/texturaterra.bmp"
          texturaAgua <- loadBMP "imagens/texturaagua.bmp"
          texturaLava <- loadBMP "imagens/texturalava.bmp"
          texturaLagrimasCristalinas <- loadBMP "imagens/texturalagrimascristalinas.bmp"
          imagemBase <- loadBMP "imagens/imagembase.bmp"
          imagemPortal <- loadBMP "imagens/imagemportal.bmp"
          imagemCarvalhoEnt <- loadBMP "imagens/imagemcarvalhoent.bmp"
          imagemArqueiroFlamejante <- loadBMP "imagens/imagemarqueiroflamejante.bmp"
          imagemSabioCorruptor <- loadBMP "imagens/imagemsabiocorruptor.bmp"
          imagemMaquinaDeResina <- loadBMP "imagens/imagemmaquinaderesina.bmp"
          imagemMagoDeGelo <- loadBMP "imagens/imagemmagodegelo.bmp"
          imagem1 <- loadBMP "imagens/1.bmp"
          imagem2 <- loadBMP "imagens/2.bmp"
          imagem3 <- loadBMP "imagens/3.bmp"
          imagemFornalhaArdente <- loadBMP "imagens/imagemfornalhaardente.bmp"
          imagemCanhao <- loadBMP "imagens/imagemcanhao.bmp"
          imagemJuizGelado <- loadBMP "imagens/imagemjuizgelado.bmp"
          imagemGharzul <- loadBMP "imagens/imagemgharzul.bmp"
          imagemBackgroundFlamejanteDiminuidaCinzenta <- loadBMP "imagens/backgroundflamejantediminuidacinzenta.bmp"
          imagemBackgroundCristalinaDiminuidaCinzenta <- loadBMP "imagens/backgroundcristalinadiminuidacinzenta.bmp"
          imagemZalrok <- loadBMP "imagens/imagemzalrok.bmp"
          imagemZalrokReforcado <- loadBMP "imagens/imagemzalrokreforcado.bmp"
          imagemFenrir <- loadBMP "imagens/imagemfenrir.bmp"
          imagemTharok <- loadBMP "imagens/imagemtharok.bmp"
          imagemAranthisMortifera <- loadBMP "imagens/imagemaranthismortifera.bmp"
          imagemDragaoDeRocha <- loadBMP "imagens/imagemdragaoderocha.bmp"
          imagemFenrirSulfr <- loadBMP "imagens/imagemfenrirsulfr.bmp"
          imagemOnyc <- loadBMP "imagens/imagemonyc.bmp"
          imagemMonstroDeMagma <- loadBMP "imagens/imagemmonstrodemagma.bmp"
          imagemReaper <- loadBMP "imagens/imagemreaper.bmp"
          imagemIgnaroth <- loadBMP "imagens/imagemignaroth.bmp"
          imagemDragaoCristalino <- loadBMP "imagens/imagemdragaocristalino.bmp"
          imagemLeaoCristalino <- loadBMP "imagens/imagemleaocristalino.bmp"
          imagemFungalith <- loadBMP "imagens/imagemfungalith.bmp"
          imagemAethralith <- loadBMP "imagens/imagemaethralith.bmp"
          texturaOuro <- loadBMP "imagens/texturaouro.bmp"
          texturaVida <- loadBMP "imagens/texturavida.bmp"
          inimigoDerrotado <- loadBMP "imagens/inimigoderrotado.bmp"
          placaMadeira <- loadBMP "imagens/placamadeira.bmp"
          tabelaMadeira <- loadBMP "imagens/tabelamadeira.bmp"
          numero1 <- loadBMP "imagens/numero1.bmp"
          numero2 <- loadBMP "imagens/numero2.bmp"
          numero3 <- loadBMP "imagens/numero3.bmp"
          numero4 <- loadBMP "imagens/numero4.bmp"
          numero5 <- loadBMP "imagens/numero5.bmp"
          numero6 <- loadBMP "imagens/numero6.bmp"
          numero7 <- loadBMP "imagens/numero7.bmp"
          numero8 <- loadBMP "imagens/numero8.bmp"
          pFogo <- loadBMP "imagens/projetilfogo.bmp"
          pGelo <- loadBMP "imagens/projetilgelo.bmp"
          pResina <- loadBMP "imagens/projetilresina.bmp"
          pFraqueza <- loadBMP "imagens/projetilfraqueza.bmp"
          pFogoCinzenta <- loadBMP "imagens/projetilfogocinzenta.bmp"
          pGeloCinzenta <- loadBMP "imagens/projetilgelocinzenta.bmp"
          pFraquezaCinzenta <- loadBMP "imagens/projetilfraquezacinzenta.bmp"
          texturaOuroCinzenta <- loadBMP "imagens/texturaourocinzenta.bmp"
          imagemMagoDeGeloCinzenta <- loadBMP "imagens/imagemmagodegelocinzenta.bmp"
          imagemFornalhaArdenteCinzenta <- loadBMP "imagens/imagemfornalhaardentecinzenta.bmp"
          imagemCanhaoCinzenta <- loadBMP "imagens/imagemcanhaocinzenta.bmp"
          imagemJuizGeladoCinzenta <- loadBMP "imagens/imagemjuizgeladocinzenta.bmp"
          texturaRocha <- loadBMP "imagens/texturarocha.bmp"
          texturaCinzas <- loadBMP "imagens/texturacinzas.bmp"
          barraVidaDeBoss <- loadBMP "imagens/barravidadeboss.bmp"
          ponteAgua <- loadBMP "imagens/ponteagua.bmp"
          ponteLagrimasCristalinas <- loadBMP "imagens/pontelagrimascristalinas.bmp"
          imagemArvore <- loadBMP "imagens/imagemarvore.bmp"
          imagemArvoreMorta <- loadBMP "imagens/imagemarvoremorta.bmp"
          imagemCristalAzul <- loadBMP "imagens/imagemcristalazul.bmp"
          imagemCristalMagenta <- loadBMP "imagens/imagemcristalmagenta.bmp"
          imagemDano <- loadBMP "imagens/imagemdano.bmp"
          imagemAlcance <- loadBMP "imagens/imagemalcance.bmp"
          imagemRajada <- loadBMP "imagens/imagemrajada.bmp"
          imagemVelocidadeDeAtaque <- loadBMP "imagens/imagemvelocidadedeataque.bmp"
          imagemProibido <- loadBMP "imagens/imagemproibido.bmp"
          imagemVelocidade <- loadBMP "imagens/imagemvelocidade.bmp"
          imagemVitoria <- loadBMP "imagens/imagemvitoria.bmp"
          imagemDerrota <- loadBMP "imagens/imagemderrota.bmp"
          imagemHelpNatural <- loadBMP "imagens/imagemhelpnatural.bmp"
          imagemHelpFlamejante <- loadBMP "imagens/imagemhelpflamejante.bmp"
          imagemHelpCristalino <- loadBMP "imagens/imagemhelpcristalino.bmp"
          imagemTexturaRelvaEscurecida3 <- loadBMP "imagens/texturarelva3.bmp"
          imagemTexturaRelvaEscurecida6 <- loadBMP "imagens/texturarelva6.bmp"
          imagemTexturaRelvaEscurecida9 <- loadBMP "imagens/texturarelva9.bmp"
          imagemTexturaRelvaEscurecida12 <- loadBMP "imagens/texturarelva12.bmp"
          imagemTexturaRelvaEscurecida15 <- loadBMP "imagens/texturarelva15.bmp"
          imagemTexturaRelvaEscurecida18 <- loadBMP "imagens/texturarelva18.bmp"
          imagemTexturaRelvaEscurecida21 <- loadBMP "imagens/texturarelva21.bmp"
          imagemTexturaTerraEscurecida3 <- loadBMP "imagens/texturaterra3.bmp"
          imagemTexturaTerraEscurecida6 <- loadBMP "imagens/texturaterra6.bmp"
          imagemTexturaTerraEscurecida9 <- loadBMP "imagens/texturaterra9.bmp"
          imagemTexturaTerraEscurecida12 <- loadBMP "imagens/texturaterra12.bmp"
          imagemTexturaTerraEscurecida15 <- loadBMP "imagens/texturaterra15.bmp"
          imagemTexturaTerraEscurecida18 <- loadBMP "imagens/texturaterra18.bmp"
          imagemTexturaTerraEscurecida21 <- loadBMP "imagens/texturaterra21.bmp"
          imagemTexturaAguaEscurecida3 <- loadBMP "imagens/texturaagua3.bmp"
          imagemTexturaAguaEscurecida6 <- loadBMP "imagens/texturaagua6.bmp"
          imagemTexturaAguaEscurecida9 <- loadBMP "imagens/texturaagua9.bmp"
          imagemTexturaAguaEscurecida12 <- loadBMP "imagens/texturaagua12.bmp"
          imagemTexturaAguaEscurecida15 <- loadBMP "imagens/texturaagua15.bmp"
          imagemTexturaAguaEscurecida18 <- loadBMP "imagens/texturaagua18.bmp"
          imagemTexturaAguaEscurecida21 <- loadBMP "imagens/texturaagua21.bmp"
          imagemTexturaLavaEscurecida15 <- loadBMP "imagens/texturalava15.bmp"
          imagemTexturaLavaEscurecida30 <- loadBMP "imagens/texturalava30.bmp"
          imagemTexturaLavaEscurecida45 <- loadBMP "imagens/texturalava45.bmp"
          imagemTexturaLavaEscurecida60 <- loadBMP "imagens/texturalava60.bmp"
          imagemTexturaLavaEscurecida75 <- loadBMP "imagens/texturalava75.bmp"
          imagemTexturaLavaEscurecida90 <- loadBMP "imagens/texturalava90.bmp"
          imagemTexturaLavaEscurecida105 <- loadBMP "imagens/texturalava105.bmp"
          imagemTexturaLagrimasCristalinasEscurecida3 <- loadBMP "imagens/texturalagrimascristalinas3.bmp"
          imagemTexturaLagrimasCristalinasEscurecida6 <- loadBMP "imagens/texturalagrimascristalinas6.bmp"
          imagemTexturaLagrimasCristalinasEscurecida9 <- loadBMP "imagens/texturalagrimascristalinas9.bmp"
          imagemTexturaLagrimasCristalinasEscurecida12 <- loadBMP "imagens/texturalagrimascristalinas12.bmp"
          imagemTexturaLagrimasCristalinasEscurecida15 <- loadBMP "imagens/texturalagrimascristalinas15.bmp"
          imagemTexturaLagrimasCristalinasEscurecida18 <- loadBMP "imagens/texturalagrimascristalinas18.bmp"
          imagemTexturaLagrimasCristalinasEscurecida21 <- loadBMP "imagens/texturalagrimascristalinas21.bmp"
          imagemTexturaCinzasEscurecida3 <- loadBMP "imagens/texturacinzas3.bmp"
          imagemTexturaCinzasEscurecida6 <- loadBMP "imagens/texturacinzas6.bmp"
          imagemTexturaCinzasEscurecida9 <- loadBMP "imagens/texturacinzas9.bmp"
          imagemTexturaCinzasEscurecida12 <- loadBMP "imagens/texturacinzas12.bmp"
          imagemTexturaCinzasEscurecida15 <- loadBMP "imagens/texturacinzas15.bmp"
          imagemTexturaCinzasEscurecida18 <- loadBMP "imagens/texturacinzas18.bmp"
          imagemTexturaCinzasEscurecida21 <- loadBMP "imagens/texturacinzas21.bmp"
          imagemTexturaRochaEscurecida3 <- loadBMP "imagens/texturarocha3.bmp"
          imagemTexturaRochaEscurecida6 <- loadBMP "imagens/texturarocha6.bmp"
          imagemTexturaRochaEscurecida9 <- loadBMP "imagens/texturarocha9.bmp"
          imagemTexturaRochaEscurecida12 <- loadBMP "imagens/texturarocha12.bmp"
          imagemTexturaRochaEscurecida15 <- loadBMP "imagens/texturarocha15.bmp"
          imagemTexturaRochaEscurecida18 <- loadBMP "imagens/texturarocha18.bmp"
          imagemTexturaRochaEscurecida21 <- loadBMP "imagens/texturarocha21.bmp"
          play janela fundo fr estadoInicial 
               (desenha imagemBackgroundNatural imagemBackgroundFlamejante imagemBackgroundCristalina imagemBackgroundFlamejanteDiminuida
                        imagemBackgroundCristalinaDiminuida imagemLogoJogo texturaRelva texturaTerra texturaAgua texturaLava texturaLagrimasCristalinas
                        imagemBase imagemPortal imagemCarvalhoEnt imagemArqueiroFlamejante imagemSabioCorruptor imagemMaquinaDeResina
                        imagemMagoDeGelo imagem1 imagem2 imagem3 imagemFornalhaArdente imagemCanhao imagemJuizGelado imagemGharzul
                        imagemBackgroundFlamejanteDiminuidaCinzenta imagemBackgroundCristalinaDiminuidaCinzenta imagemZalrok imagemZalrokReforcado
                        imagemFenrir imagemTharok imagemAranthisMortifera imagemDragaoDeRocha imagemFenrirSulfr imagemOnyc imagemMonstroDeMagma
                        imagemReaper imagemIgnaroth imagemDragaoCristalino imagemLeaoCristalino imagemFungalith imagemAethralith texturaOuro
                        texturaVida inimigoDerrotado placaMadeira tabelaMadeira numero1 numero2 numero3 numero4 numero5 numero6 numero7 numero8
                        pFogo pGelo pResina pFraqueza pFogoCinzenta pGeloCinzenta pFraquezaCinzenta texturaOuroCinzenta imagemMagoDeGeloCinzenta 
                        imagemFornalhaArdenteCinzenta imagemCanhaoCinzenta imagemJuizGeladoCinzenta texturaRocha texturaCinzas barraVidaDeBoss
                        ponteAgua ponteLagrimasCristalinas imagemArvore imagemArvoreMorta imagemCristalAzul imagemCristalMagenta
                        imagemDano imagemAlcance imagemRajada imagemVelocidadeDeAtaque imagemProibido imagemVelocidade imagemVitoria 
                        imagemDerrota imagemHelpNatural imagemHelpFlamejante imagemHelpCristalino imagemTexturaRelvaEscurecida3
                        imagemTexturaRelvaEscurecida6 imagemTexturaRelvaEscurecida9 imagemTexturaRelvaEscurecida12
                        imagemTexturaRelvaEscurecida15 imagemTexturaRelvaEscurecida18 imagemTexturaRelvaEscurecida21
                        imagemTexturaTerraEscurecida3 imagemTexturaTerraEscurecida6 imagemTexturaTerraEscurecida9
                        imagemTexturaTerraEscurecida12 imagemTexturaTerraEscurecida15 imagemTexturaTerraEscurecida18
                        imagemTexturaTerraEscurecida21 imagemTexturaAguaEscurecida3 imagemTexturaAguaEscurecida6
                        imagemTexturaAguaEscurecida9 imagemTexturaAguaEscurecida12 imagemTexturaAguaEscurecida15
                        imagemTexturaAguaEscurecida18 imagemTexturaAguaEscurecida21 imagemTexturaLavaEscurecida15
                        imagemTexturaLavaEscurecida30 imagemTexturaLavaEscurecida45 imagemTexturaLavaEscurecida60
                        imagemTexturaLavaEscurecida75 imagemTexturaLavaEscurecida90 imagemTexturaLavaEscurecida105
                        imagemTexturaLagrimasCristalinasEscurecida3 imagemTexturaLagrimasCristalinasEscurecida6 imagemTexturaLagrimasCristalinasEscurecida9
                        imagemTexturaLagrimasCristalinasEscurecida12 imagemTexturaLagrimasCristalinasEscurecida15 imagemTexturaLagrimasCristalinasEscurecida18
                        imagemTexturaLagrimasCristalinasEscurecida21 imagemTexturaCinzasEscurecida3 imagemTexturaCinzasEscurecida6
                        imagemTexturaCinzasEscurecida9 imagemTexturaCinzasEscurecida12 imagemTexturaCinzasEscurecida15
                        imagemTexturaCinzasEscurecida18 imagemTexturaCinzasEscurecida21 imagemTexturaRochaEscurecida3
                        imagemTexturaRochaEscurecida6 imagemTexturaRochaEscurecida9 imagemTexturaRochaEscurecida12
                        imagemTexturaRochaEscurecida15 imagemTexturaRochaEscurecida18 imagemTexturaRochaEscurecida21)
               reageEventos reageTempo