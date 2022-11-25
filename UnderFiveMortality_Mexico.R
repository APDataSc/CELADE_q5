#**************************************************************************************#
#**************************************************************************************#
#
#               Rutina para la estimación de Tasas de Mortalidad en la Niñez                        
#                Curso Análisis Demográfico para el Desarrollo Sostenible
#
#     Fecha de elaboración:   07/11/2022
#     Última actualización:   17/11/2022
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (agpena@colmex.mx)
#     Organización:           El Colegio de México, A.C.
#                             
#
#**************************************************************************************#
#**************************************************************************************#

## Paquetes ####
rm(list = ls())
# install.packages("u5mr")
library(u5mr)
library(data.table)
library(reshape2)
library(ggrepel)
library(dplyr)
# ?u5mr_trussell


## Carga de las bases de datos - son solo dos archivos *.csv ####

setwd("C:\\Users\\LENOVO\\OneDrive - El Colegio de México A.C\\CREAD 2022\\Material\\Trabalho final")

mexico <- 
  fread( 'base_q5.csv', encoding="UTF-8") %>%
  .[ anio %in% c(2010, 2020) ]
names(mexico)

igme <- 
  fread( 'igme_q5.csv', encoding="UTF-8")
names(igme)
table(igme$name)


#**************************************************************************************#

## Cálculos de tasas a nivel nacional con la función "u5mr_trussell" ####

"Bases Nacional 2010, 2020"
mex_nac_20 <- mexico[anio==2020 & entidad=="Nacional"] 
mex_nac_10 <- mexico[anio==2010 & entidad=="Nacional"] 

"q5 2020"
mex_nac20_q5 <- u5mr_trussell(
  mex_nac_20,
  women = "women",
  child_born = "child_born",
  child_dead = "child_dead",
  agegrp = "agegrp",
  model = "west",
  svy_year = 2020+2/12+15/365,
  sex = "both"
)

"q5 2010"
mex_nac10_q5 <- u5mr_trussell(
  mex_nac_10,
  women = "women",
  child_born = "child_born",
  child_dead = "child_dead",
  agegrp = "agegrp",
  model = "west",
  svy_year = 2010+5/12+12/365,
  sex = "both"
)

"q5 2020 mal calculado"
mex_nac20_q5x <- u5mr_trussell(
  mex_nac_20,
  women = "women",
  child_born = "child_born_13",
  child_dead = "child_dead",
  agegrp = "agegrp",
  model = "west",
  svy_year = 2020+2/12+15/365,
  sex = "both"
)



"Gráfica a nivel nacional de las tasas estimadas y fuentes del IGME"

with(mex_nac20_q5,
     plot(year, q5*1000, type = "b", pch = 19,
          ylim = c(10, 55),
          xlim = c(1995, 2020),
          bty = "n",
          col = "antiquewhite3", xlab = "Reference date", ylab = "u5MR (per 100)",
          main = paste0("Under-five mortality, q(5) in México, model West\n",
                        "and the Trussell Version of the Brass method")))
with(mex_nac10_q5,
     lines(year, q5*1000, pch = 19, col = "blue", type = "b"))
     lines(igme[name=="IGME", year, ], igme[igme$name=="IGME", q5], 
      pch = 19, col = "black", type = "b")
# with(mex_nac20_q5x,
#       lines(year, q5*1000, pch = 18, col = "red", type = "b", lty = 2))
     lines(igme[name=="CENSO_10", year, ], igme[igme$name=="CENSO_10", q5], 
           pch = 18, col = "skyblue", type = "b", lty = 2)
     lines(igme[name=="CONTEO_05", year, ], igme[igme$name=="CONTEO_05", q5], 
           pch = 18, col = "pink", type = "b", lty = 2)
     lines(igme[name=="CONTEO_00", year, ], igme[igme$name=="CONTEO_00", q5], 
           pch = 18, col = "purple", type = "b", lty = 2)
     lines(igme[name=="ENADID_06", year, ], igme[igme$name=="ENADID_06", q5], 
           pch = 18, col = "brown", type = "b", lty = 2)
     lines(igme[name=="ENADID_09", year, ], igme[igme$name=="ENADID_09", q5], 
           pch = 18, col = "aquamarine", type = "b", lty = 2)
     lines(igme[name=="ENADID_14e", year, ], igme[igme$name=="ENADID_14e", q5], 
           pch = 18, col = "aquamarine4", type = "b", lty = 2)
     lines(igme[name=="ENADID_14", year, ], igme[igme$name=="ENADID_14", q5], 
           pch = 18, col = "darkblue", type = "b", lty = 2)
     lines(igme[name=="ENADID_18", year, ], igme[igme$name=="ENADID_18", q5], 
           pch = 18, col = "orange", type = "b", lty = 2)
     lines(igme[name=="INTER_15", year, ], igme[igme$name=="INTER_15", q5], 
           pch = 18, col = "chartreuse", type = "b", lty = 2)
     lines(igme[name=="VR", year, ], igme[igme$name=="VR", q5], 
           pch = 18, col = "gray", type = "b", lty = 3)
     lines(igme[name=="VR_IGME", year, ], igme[igme$name=="VR_IGME", q5], 
           pch = 18, col = "azure2", type = "b", lty = 3)
with(mex_nac20_q5, text(year, q5*1000, agegrp, cex=0.5, pos=3,col="antiquewhite3"))
with(mex_nac10_q5, text(year, q5*1000, agegrp, cex=0.5, pos=3,col="blue"))
legend("topright", legend=c("CENSO_20", "CENSO_10",
                            "IGME_Adjust", "CENSO_10IG", 
                            "CONTEO_05", "CONTEO_00", 
                            "ENADID_06", "ENADID_09", 
                            "ENADID_14ex", "ENADID_14", 
                            "ENADID_18", "INTER_15",
                            "VR", "VR_IGME"),
       col = c("antiquewhite3", "blue", "black", "skyblue", "pink", 
               "purple", "brown", "aquamarine", "aquamarine4", "darkblue", 
               "orange", "chartreuse", "gray", "azure2"),
       lty = c(1,1,1,rep(3,11)), cex=0.5, ncol = 4)


# legend("bottomright", legend=c("CENSO_20_est", "CENSO_10_est",
#                             "CENSO_20_mal", "CENSO_10_IGME"),
#        col = c("antiquewhite3", "blue", "red", "skyblue"),
#        lty = c(1,1,rep(3,2)), cex=0.65, ncol = 2)



#**************************************************************************************#

## Estimaciones por entidad federativa ####

"Entidades federativas q5"
mex_ent <- data.table()

for( a in c( 2010, 2020 ) ){
  n <- 0
  
  for( e in unique( mexico$entidad )[-1] ){
    n=n+1
    
    if(a == 2010) {
      svy_year = 2010+5/12+12/365
    } else {
      svy_year = 2020+2/12+15/365
    }  
    
    "Cálculo q5"
    mex_20_q5 <- u5mr_trussell(
      mexico[anio==a & entidad==e],
      women = "women",
      child_born = "child_born",
      child_dead = "child_dead",
      agegrp = "agegrp",
      model = "west",
      svy_year = svy_year,
      sex = "both"
    ) %>% as.data.table()
    
    mex_ent <- rbind(mex_ent,
                     mex_20_q5[ , .(n=n, 
                                    ent=e, 
                                    Censo=as.factor(a), 
                                    edad=agegrp,
                                    year, 
                                    q5=q5*1000)])
  }
}


## Gráficas de panel por entidad federativa

"Primeras 16 entidades"
  ggplot( mex_ent[n<=16]) +
  geom_point( aes( x = year, y = q5, color = Censo),
              size = 1.25 ) +
  geom_line( aes( x = year, y = q5, color = Censo, linetype = Censo ),
             size = 1 ) +
  facet_wrap( ~ ent, nrow = 8, ncol=4 ) +
  theme_bw() +
  theme(legend.position = 'top') +
  ylim(min(mex_ent$q5), max(mex_ent$q5)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("Año") +
  ylab("Tasa de mortalidad en la niñez q5 (por 1000)")   

"Segundas 16 entidades"
  ggplot( mex_ent[n>16]) +
    geom_point( aes( x = year, y = q5, color = Censo),
                size = 1.25 ) +
    geom_line( aes( x = year, y = q5, color = Censo, linetype = Censo ),
               size = 1 ) +
    facet_wrap( ~ ent, nrow = 8, ncol=4 ) +
    theme_bw() +
    theme(legend.position = 'top') +
    ylim(min(mex_ent$q5), max(mex_ent$q5)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    xlab("Año") +
    ylab("Tasa de mortalidad en la niñez q5 (por 1000)")

# write.table(mex_ent, "clipboard", sep="\t", row.names=F)
  


#**************************************************************************************#

## Diagrama de dispersión de las tasas de la edad "20-24" de 2010 vs 2020 ####

mex_ent_2024 <- dcast(mex_ent[edad=="20-24"], ent+n~Censo, mean) %>% 
                setDT %>% 
                .[ , Comportamiento := ifelse(`2020`/`2010`< 1, "Reducción", 
                                              "Aumento")] 

ggplot(mex_ent_2024, aes(x=`2010`, y=`2020`), col="purple") + 
  geom_point() +
  ylim(16, 30) +
  xlim(16, 30) +
  geom_text_repel(aes(label=c("AGU", "BC", "BCS", "CAM", "CHIA", "CHIH", "CDMX", "COA",
                    "COL", "DUR", "GUA", "GUE", "HID", "JAL", "MEX", "MIC",
                    "MOR", "NAY", "NL", "OAX", "PUE", "QUE", "QUI", "SLP",
                    "SIN", "SON", "TAB", "TAM", "TLA", "VER", "YUC", "ZAC"),
                    color=Comportamiento
                    ), 
                  size=2.5) +
  geom_abline(intercept = 0, slope = 1, color="purple", 
              linetype="dashed", size=1)  

# FIN ####