##############################################################################
######################### FUENTE DE INGRESO ##################################
##############################################################################

DR_descompSK <- function(A, B, A_SK, B_SK, def,lp1_urb, lp1_rur, lp2_urb,lp2_rur,Ilp1_urb, Ilp1_rur, Ilp2_urb,Ilp2_rur) {
  
  library(tidyverse)
  library(haven)
  library(survey)
  
  # FUNCIÃ“N PARA ESTIMAR POBREZA POR INGRESOS
  pobrezaing <- function(base) {
    baseX <- mutate(base,
                    # Se identifica a los hogares bajo lp1
                    plp_e=case_when(ictpc <lp1_urb  & rururb==0  ~ 1,
                                    ictpc>=lp1_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                                    ictpc <lp1_rur  & rururb==1  ~ 1,
                                    ictpc>=lp1_rur  & rururb==1 & !is.na(ictpc) ~ 0),
                    # Se identifica a los hogares bajo lp2
                    plp =case_when(ictpc <lp2_urb  & rururb==0  ~ 1,
                                   ictpc>=lp2_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                                   ictpc <lp2_rur  & rururb==1  ~ 1,
                                   ictpc>=lp2_rur  & rururb==1 & !is.na(ictpc) ~ 0))
  }
  
  # Preparamos los objetos de diseÃ±o de muestra
  svyA <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = A)
  svyB <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = B)
  svyA_SPS <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = A_SK)
  svyB_SPS <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = B_SK)
  
  # Datos observados con y sin programas sociales
  ObsA <- as.numeric(svymean(~plp+plp_e, design = svyA))
  ObsB <- as.numeric(svymean(~plp+plp_e, design = svyB))
  ObsA_SPS <- as.numeric(svymean(~plp+plp_e, design = svyA_SPS))
  ObsB_SPS <- as.numeric(svymean(~plp+plp_e, design = svyB_SPS))
  
  # Diferencia entre datos con y sin programas sociales
  DifA <- (ObsA - ObsA_SPS)
  DifB <- (ObsB - ObsB_SPS)
  
  # Diferencia de la diferencia a travÃ©s de los aÃ±os
  Dif_PS <- DifB - DifA
  
  # Obtenemos el cambio absoluto en pobreza
  dif <- (ObsB - ObsA)
  
  ############################ EFECTO CRECIMIENTO ###############################
  
  # Deflactamos los ingresos de B a pesos de A
  baseB_EC <- mutate(B_SK, ictpc=ictpc/def)
  
  # Nuevo objeto de diseÃ±o
  svyB_EC <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseB_EC)
  
  # Tasa de crecimiento real del ictpc entre A y B
  rate_A_B <- as.numeric(svymean(~ictpc, design = svyB_EC)/svymean(~ictpc, design = svyA_SPS))
  rate_B_A <- as.numeric(svymean(~ictpc, design = svyA_SPS)/svymean(~ictpc, design = svyB_EC))
  
  # Aplicamos la tasa de creicImiento real del ictpc al los ingresos de A
  baseA_EC <- mutate(A_SK, ictpc = ictpc*rate_A_B)
  
  # Calculamos pobreza con lÃ­neas de A 
  baseEC <- pobrezaing(baseA_EC)
  
  # Nuevo objeto de diseÃ±o
  svyEC <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseEC)
  
  EC <- as.numeric(svymean(~plp+plp_e, design = svyEC))
  G <-(EC - ObsA_SPS)
  
  ######################### EFECTO REDISTRIBUCIÃ“N ##############################
  # Deflactamos a pesos de A y aplicamos la tasa de crecimiento de B/A
  baseB_ERD <- mutate(B_SK, ictpc = (ictpc/def)*rate_B_A)
  
  baseB_ERD <- pobrezaing(baseB_ERD)
  
  svy_ERD <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseB_ERD)
  
  ERD <- as.numeric(svymean(~plp+plp_e, design = svy_ERD))
  RD <-(ERD - ObsA_SPS)
  
  ######################### EFECTO INFLACIÃ“N ###################################
  
  baseA_EI <- mutate(A_SK, ictpc = ictpc*def)
  
  baseA_EI <- mutate(baseA_EI,
                     # Se identifica a los hogares bajo lp1
                     plp_e=case_when(ictpc <Ilp1_urb  & rururb==0  ~ 1,
                                     ictpc>=Ilp1_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                                     ictpc <Ilp1_rur  & rururb==1  ~ 1,
                                     ictpc>=Ilp1_rur  & rururb==1 & !is.na(ictpc) ~ 0),
                     # Se identifica a los hogares bajo lp2
                     plp =case_when(ictpc <Ilp2_urb  & rururb==0  ~ 1,
                                    ictpc>=Ilp2_urb  & rururb==0 & !is.na(ictpc) ~ 0,
                                    ictpc <Ilp2_rur  & rururb==1  ~ 1,
                                    ictpc>=Ilp2_rur  & rururb==1 & !is.na(ictpc) ~ 0))
  
  svy_EI <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseA_EI)
  
  EI <- as.numeric(svymean(~plp+plp_e, design = svy_EI))
  I <- (EI - ObsA_SPS)
  
  # Generamos la de resultados y el residual 
  results <- round(data.frame("A" = ObsA, "B" = ObsB, "Cambio" = dif, "Crecimiento" = G, 
                              "RedistribuciÃ³n" = RD, "InflaciÃ³n" = I, "Programas Sociales" = Dif_PS)*100,1)
  results <- mutate(results, Residual = Cambio - (Crecimiento + RedistribuciÃ³n + InflaciÃ³n + Programas.Sociales))
  rownames(results) <- c("Pobreza por ingresos", "Pobreza extrema por ingresos")
  results
  
}