##############################################################################
######################### EFECTO INFLACIÓN ###################################
##############################################################################

DR_descompI <- function(A, B, def,lp1_urb, lp1_rur, lp2_urb,lp2_rur,Ilp1_urb, Ilp1_rur, Ilp2_urb,Ilp2_rur) {
  
  library(tidyverse)
  library(haven)
  library(survey)
  
  # FUNCIÓN PARA ESTIMAR POBREZA POR INGRESOS
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
  
  # Fijamos los objetos de diseÃ±o muestral para las bases originales
  svyA <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = A)
  svyB <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = B)
  
  # Dato observado para A (aÃ±o inicial/de referencia)
  ObsA <- as.numeric(svymean(~plp+plp_e, design = svyA))
  ObsB <- as.numeric(svymean(~plp+plp_e, design = svyB))
  
  # Obtenemos el cambio absoluto en pobreza
  dif <- (ObsB - ObsA)
  
  ############################ EFECTO CRECIMIENTO ###############################
  # Deflactamos los ingresos de B a pesos de A
  baseB_EC <- mutate(B, ictpc=ictpc/def)
  
  # Nuevo objeto de diseÃ±o
  svyB_EC <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseB_EC)
  
  # Tasa de crecimiento real del ictpc entre A y B
  rate_A_B <- as.numeric(svymean(~ictpc, design = svyB_EC)/svymean(~ictpc, design = svyA))
  rate_B_A <- as.numeric(svymean(~ictpc, design = svyA)/svymean(~ictpc, design = svyB_EC))
  
  # Aplicamos la tasa de creicImiento real del ictpc al los ingresos de A
  baseA_EC <- mutate(A, ictpc = ictpc*rate_A_B)
  
  # Calculamos pobreza con líneas de A 
  baseEC <- pobrezaing(baseA_EC)
  
  # Nuevo objeto de diseÃ±o
  svyEC <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseEC)
  
  EC <- as.numeric(svymean(~plp+plp_e, design = svyEC))
  G <-(EC - ObsA)
  
  ######################### EFECTO REDISTRIBUCIÃ“N ##############################
  # Deflactamos a pesos de A y aplicamos la tasa de crecimiento de B/A
  baseB_ERD <- mutate(B, ictpc = (ictpc/def)*rate_B_A)
  
  baseB_ERD <- pobrezaing(baseB_ERD)
  
  svy_ERD <- svydesign(ids = ~upm , weights = ~factor, strata=~est_dis, data = baseB_ERD)
  
  ERD <- as.numeric(svymean(~plp+plp_e, design = svy_ERD))
  RD <-(ERD - ObsA)
  
  ######################### EFECTO INFLACIÃ“N ###################################
  
  baseA_EI <- mutate(A, ictpc = ictpc*def)
  
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
  I <- (EI - ObsA)
  
  # Generamos la de resultados y el residual 
  results <- round(data.frame("A" = ObsA, "B" = ObsB, "Cambio" = dif, "Crecimiento" = G, "RedistribuciÃ³n" = RD, "InflaciÃ³n" = I)*100,1)
  results <- mutate(results, Residual = Cambio - (Crecimiento + RedistribuciÃ³n + InflaciÃ³n))
  rownames(results) <- c("Pobreza por ingresos", "Pobreza extrema por ingresos")
  results
  
}  

