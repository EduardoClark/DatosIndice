Datos <- read.csv("data/Datos.csv",  stringsAsFactors = FALSE)
Datos <- subset(Datos, select=c(1:8,10,11,12,13,14,15,18,33,46,48,51,63,64,94,96,97,104,105))
Datos$CapitalCapita <- Datos$Formación.bruta.de.capital.fijo / Datos$Población
Datos$HomicidiosTasa <- Datos$Homicidios / Datos$Población * 100000
Datos$Anio <- c(rep(2001, 32), rep(2002, 32), rep(2003, 32), rep(2004, 32),rep(2005, 32),
                                rep(2006, 32), rep(2007, 32), rep(2008, 32),
                                rep(2009, 32), rep(2010, 32))
                
Datos <- arrange(Datos, Datos$Indicador)
Datos$GrowthCapitalCapita <- c(5, Datos$CapitalCapita[-320])
Datos$GrowthCapitalCapita <- ((Datos$CapitalCapita - Datos$GrowthCapitalCapita) / Datos$GrowthCapitalCapita) *100

Datos$GrowthHomicidosTasa <- c(5, Datos$HomicidosTasa[-320])                
Datos$GrowthHomicidosTasa <- ((Datos$HomicidiosTasa - Datos$GrowthHomicidosTasa) / Datos$GrowthHomicidosTasa) * 100             
                            
Datos$GrowthTalento <- c(5, Datos$Talento[-320])                
Datos$GrowthTalento <- ((Datos$Talento - Datos$GrowthTalento) / Datos$GrowthTalento) * 100 
                
 
Datos$GrowthIncidencia.delictiva <- c(5, Datos$Incidencia.delictiva[-320])                
Datos$GrowthIncidencia.delictiva <- ((Datos$Incidencia.delictiva - Datos$GrowthIncidencia.delictiva) / Datos$GrowthIncidencia.delictiva) * 100   

Datos$GrowthPercepción.sobre.inseguridad <- c(5, Datos$Percepción.sobre.inseguridad[-320])                
Datos$GrowthPercepción.sobre.inseguridad <- ((Datos$Percepción.sobre.inseguridad - Datos$GrowthPercepción.sobre.inseguridad) / Datos$GrowthPercepción.sobre.inseguridad) * 100
                
Datos$GrowthAtracción.de.talento <- c(5, Datos$Atracción.de.talento[-320])                
Datos$GrowthAtracción.de.talento <- ((Datos$Atracción.de.talento - Datos$GrowthAtracción.de.talento) / Datos$GrowthAtracción.de.talento) * 100 

Datos <- subset(Datos, Datos$Anio != 2001)

Datos1 <- subset(Datos, Datos$GrowthTalento != 0)
                
fixed <-plm(GrowthCapitalCapita ~ GrowthHomicidosTasa + GrowthIncidencia.delictiva + GrowthPercepción.sobre.inseguridad  , data=Datos, index=c("Indicador", "Anio"), model="within")
fixed1 <-plm(GrowthTalento~ GrowthHomicidosTasa + GrowthIncidencia.delictiva + GrowthPercepción.sobre.inseguridad  , data=Datos, index=c("Indicador", "Anio"), model="within")
