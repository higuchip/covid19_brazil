library(dplyr)

covid19
novos_casos<-diff(covid19$casos)

novos_casos[-1] / novos_casos[-length(novos_casos)]

covid19_2<-covid19

covid19_2["novos_casos"]<-c(0,diff(covid19$casos))


