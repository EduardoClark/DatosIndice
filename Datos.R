##
Datos <- read.csv("data/Datos.csv",  stringsAsFactors = FALSE)
Datos$Anio <- c(rep(2001, 32), rep(2002, 32), rep(2003, 32), rep(2004, 32),rep(2005, 32),
                                                  rep(2006, 32), rep(2007, 32), rep(2008, 32),
                                                  rep(2009, 32), rep(2010, 32))



Datos$Homicidios <- Datos$Homicidios / Datos$Población
Datos$Formación.bruta.de.capital.fijo <- Datos$Formación.bruta.de.capital.fijo



##Create Growth rates
require(plyr)
Datos <- arrange(Datos, Datos$Indicador)
Datos$CrecimientoCapital <- 1
for (i in 2:320){
 tmp1 <- Datos$Formación.bruta.de.capital.fijo[i]
 tmp2 <- Datos$Formación.bruta.de.capital.fijo[i - 1]
 tmp <- ((tmp1 - tmp2 ) / tmp2) * 100
 Datos$CrecimientoCapital[i] <- tmp
 remove(list=ls(pattern="tmp"))
}
Datos$CrecimientoTalento <- 1
for (i in 2:320){
  tmp1 <- Datos$Talento[i]
  tmp2 <- Datos$Talento[i - 1]
  tmp <- ((tmp1 - tmp2 ) / tmp2) * 100
  Datos$CrecimientoTalentol[i] <- tmp
  remove(list=ls(pattern="tmp"))
}
Datos$CrecimientoHomicidios <- 1
for (i in 2:320){
  tmp1 <- Datos$Homicidios[i]
  tmp2 <- Datos$Homicidios[i - 1]
  tmp <- ((tmp1 - tmp2 ) / tmp2) * 100
  Datos$CrecimientoHomicidios[i] <- tmp
  remove(list=ls(pattern="tmp"))
}
Datos$CrecimientoInseguridad <- 1
for (i in 2:320){
  tmp1 <- Datos$Percepción.sobre.inseguridad[i]
  tmp2 <- Datos$Percepción.sobre.inseguridad[i - 1]
  tmp <- ((tmp1 - tmp2 ) / tmp2) * 100
  Datos$CrecimientoInseguridad[i] <- tmp
  remove(list=ls(pattern="tmp"))
}
