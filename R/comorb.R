#' Comorbilidades
#'
#' modified version of comorbidity to work simultaneously with icd9 and icd10
#'
#' @name comorb
#'
#' @param tabla
#'
#' @return modified version of comorbidity
#'
#' @examples
#'
#'
#' @export





comorb<-function(tabla ="//Centcs01srv03/TABLEAU/Coronavirus/R/Nuevos_positivos.csv",id="Pac_Unif_Cod",code="Diag_Cod",score = "charlson", assign0=TRUE)
{
  #require(lubridate) #después veremos cómo encajamos las fechas
  require(comorbidity)
  require(readxl)
  require(dplyr)

  tablacie9<-tabla%>%
    filter(Diag_Tipo=="9")%>%
    dplyr::select(id,code)#por ahora estoy eliminando fecha pero después la vamos a necesitar
  tablacie10<-tabla%>%
    filter(Diag_Tipo=="10ES")%>%
    dplyr::select(id,code)

  comorbidity(tablacie9,id = id,code = code,icd="icd9",score = score,assign0 = TRUE,tidy.codes=TRUE)->comor9
  comorbidity(tablacie10,id = id,code = code,icd="icd10",score = score,assign0 = TRUE,tidy.codes=TRUE)->comor10

  hfh<-rbind(comor9,comor10)%>%
    group_by(Pac_Unif_Cod)%>%
    summarise_at(vars(c("ami","chf","pvd","cevd","dementia","copd","rheumd","pud","mld","diab","diabwc","hp","rend", "canc","msld","metacanc","aids")),list(any))

    data.table::setDF(hfh)
    if (score == "charlson") {
      hfh$score <- with(hfh, ami + chf + pvd + cevd + dementia +
                          copd + rheumd + pud + mld * ifelse(msld == 1 & assign0,
                                                             0, 1) + diab * ifelse(diabwc == 1 & assign0, 0, 1) +
                          diabwc + hp + rend + canc * ifelse(metacanc == 1 &
                                                               assign0, 0, 1) + msld + metacanc + aids)
      hfh$index <- with(hfh, cut(score, breaks = c(0, 1, 2.5, 4.5,
                                                   Inf), labels = c("0", "1-2", "3-4",
                                                                    ">=5"), right = FALSE))
      hfh$wscore <- with(hfh, ami + chf + pvd + cevd + dementia +
                           copd + rheumd + pud + mld * ifelse(msld == 1 & assign0,
                                                              0, 1) + diab * ifelse(diabwc == 1 & assign0, 0, 1) +
                           diabwc * 2 + hp * 2 + rend * 2 + canc * ifelse(metacanc ==
                                                                            1 & assign0, 0, 2) + msld * 3 + metacanc * 6 + aids *
                           6)
      hfh$windex <- with(hfh, cut(wscore, breaks = c(0, 1, 2.5,
                                                     4.5, Inf), labels = c("0", "1-2", "3-4",
                                                                           ">=5"), right = FALSE))
    }
    else {
      hfh$score <- with(hfh, chf + carit + valv + pcd + pvd + hypunc *
                          ifelse(hypc == 1 & assign0, 0, 1) + hypc + para +
                          ond + cpd + diabunc * ifelse(diabc == 1 & assign0,
                                                       0, 1) + diabc + hypothy + rf + ld + pud + aids +
                          lymph + metacanc + solidtum * ifelse(metacanc ==
                                                                 1 & assign0, 0, 1) + rheumd + coag + obes + wloss +
                          fed + blane + dane + alcohol + drug + psycho + depre)
      hfh$index <- with(hfh, cut(score, breaks = c(-Inf, 0, 1,
                                                   4.5, Inf), labels = c("<0", "0", "1-4",
                                                                         ">=5"), right = FALSE))
      hfh$wscore_ahrq <- with(hfh, chf * 9 + carit * 0 + valv *
                                0 + pcd * 6 + pvd * 3 + ifelse(hypunc == 1 | hypc ==
                                                                 1, 1, 0) * (-1) + para * 5 + ond * 5 + cpd * 3 +
                                diabunc * ifelse(diabc == 1 & assign0, 0, 0) + diabc *
                                (-3) + hypothy * 0 + rf * 6 + ld * 4 + pud * 0 +
                                aids * 0 + lymph * 6 + metacanc * 14 + solidtum *
                                ifelse(metacanc == 1 & assign0, 0, 7) + rheumd *
                                0 + coag * 11 + obes * (-5) + wloss * 9 + fed * 11 +
                                blane * (-3) + dane * (-2) + alcohol * (-1) + drug *
                                (-7) + psycho * (-5) + depre * (-5))
      hfh$wscore_vw <- with(hfh, chf * 7 + carit * 5 + valv * (-1) +
                              pcd * 4 + pvd * 2 + ifelse(hypunc == 1 | hypc ==
                                                           1, 1, 0) * 0 + para * 7 + ond * 6 + cpd * 3 + diabunc *
                              ifelse(diabc == 1 & assign0, 0, 0) + diabc * 0 +
                              hypothy * 0 + rf * 5 + ld * 11 + pud * 0 + aids *
                              0 + lymph * 9 + metacanc * 12 + solidtum * ifelse(metacanc ==
                                                                                  1 & assign0, 0, 4) + rheumd * 0 + coag * 3 + obes *
                              (-4) + wloss * 6 + fed * 5 + blane * (-2) + dane *
                              (-2) + alcohol * 0 + drug * (-7) + psycho * 0 + depre *
                              (-3))
      hfh$windex_ahrq <- with(hfh, cut(wscore_ahrq, breaks = c(-Inf,
                                                               0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                           "1-4", ">=5"), right = FALSE))
      hfh$windex_vw <- with(hfh, cut(wscore_vw, breaks = c(-Inf,
                                                           0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                       "1-4", ">=5"), right = FALSE))
    }

    return(hfh)
}

