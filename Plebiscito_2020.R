

#### Columna #### 

# Alumnos/a: Daniel Contreras, Emilio Mun~oz,Francisca Moya # 

#### Se analizara base de datos que contiene informacion acerca  ####
#   votacion en plebiscito de 2020 en Chile 


#### Preparacion de paquetes y base #### 

# Instalacion de paquetes y librerias

install.packages('ggcorrplot')
install.packages('factoextra')
install.packages('modelsummary')

library(readr)
library(pca3d)
library(plotly)
library(dplyr)
library(psych)
library(ggcorrplot)
library(factoextra)
library(modelsummary)

# Lectura de datos

#url <-  https://www.kaggle.com/tagotero/chile-plebiscito-2020-participacion-servel 

votaciones <-  read_delim("VW_VOTARON_2020PLEB_Datos completos.csv", 
                           ";", escape_double = FALSE, na = "NA", 
                           trim_ws = TRUE)
votaciones <- as.data.frame(votaciones)


#### 1.	Describir las variables ####

# Caracteristicas de base de datos

dim(votaciones)
# 14855719       17

# La base contiene 14.855.719  observaciones y 17 variables

str(votaciones)

glimpse(votaciones)

#Rows: 14,855,719
#Columns: 17
#$ c               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, …
#$ Circunscripcion       <chr> "Coyhaique", "El Puerto"…
#$ Comuna                <chr> "Coyhaique", "Valparaiso…
#$ DV                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, …
#$ Edad                  <dbl> 22, 89, 99, 22, 95, 31, …
#$ Nacionalidad          <chr> "chilena", "chilena", "c…
#$ `Pais Domicilio`      <chr> "Chile", "Chile", "Chile…
#$ `Pais Nacimiento`     <chr> "Chile", "Chile", "Chile…
#$ Partido               <chr> "[130] FEDERACION REGION…
#$ Provincia             <chr> "Coyhaique", "Valparaiso…
#$ `Rango Edad`          <chr> "20-24", "80 o +", "80 o…
#$ Region                <chr> "De Aysen Del General Ca…
#$ Sexo                  <chr> "femenino", "femenino", …
#$ Sufragio              <chr> "sufragó", "no sufragó",…
#$ VotoExterior          <chr> "Nacional", "Nacional", …
#$ `Número de registros` <dbl> 1, 1, 1, 1, 1, 1, 1, 1, …
#$ Votaron               <dbl> 1, NA, NA, 1, NA, 1, NA,…

# Se aprecia que de las 17 variables 9 son numericas, y 1 de caracter.

# Convertir a factor 
votaciones$Votaron <- if_else(votaciones$Votaron == 1, "Si",'No')

votaciones$Votaron         <- as.factor(votaciones$Votaron)

# Igualmente se considera que la variable sufragio contiene la misma informacion
# por lo que se opta por continuar con esta variable


# Convertir a factor 

votaciones$Circunscripcion <- as.factor(votaciones$Circunscripcion)
votaciones$Comuna          <- as.factor(votaciones$Comuna)
votaciones$Sexo            <- as.factor(votaciones$Sexo)
votaciones$Nacionalidad    <- as.factor(votaciones$Nacionalidad)
votaciones$Partido         <- as.factor(votaciones$Partido)
votaciones$Region          <- as.factor(votaciones$Region)
votaciones$`Pais Nacimiento`<- as.factor(votaciones$`Pais Nacimiento`)
votaciones$Sufragio        <- as.factor(votaciones$Sufragio)
votaciones$`Rango Edad`    <- as.factor(votaciones$`Rango Edad`)
votaciones$VotoExterior    <- as.factor(votaciones$VotoExterior)
votaciones$`Pais Domicilio`<- as.factor(votaciones$`Pais Domicilio`)


glimpse(votaciones)

summary (votaciones)

#    Circunscripcion    
#  Puente Alto :  317104  
#  San Bernardo:  223754  
#  Maipu       :  218144  
#  Talca       :  179416  
#  El Centro   :  176635  
#  Providencia :  169757  
#  (Other)     :13570909  

#     Comuna                     Edad       
#Puente Alto :  398965      Min.   : 18.00  
#Maipu       :  389595      1st Qu.: 31.00  
#Santiago    :  337288      Median : 45.00  
#La Florida  :  307743      Mean   : 46.53  
#Viña Del Mar:  301101      3rd Qu.: 59.00  
#Valparaiso  :  290851      Max.   :138.00  
#(Other)     :12830176    

# Nacionalidad             Pais Domicilio    
#chilena   :14476890   Chile         :14796197  
#extranjero:  378829   Argentina     :    8778  
#                      Estados Unidos:    7597  
#                      España        :    6267  
#                      Australia     :    4451  
#                      Alemania      :    3911  
#                      (Other)       :   28518  

#Pais Nacimiento    
#Chile    :14476890  
#Peru     :  138381  
#Bolivia  :   43440  
#Colombia :   42048  
#Argentina:   28563  
#Ecuador  :   15993  
#(Other)  :  110404  

#Partido        
#SIN PARTIDO                      :13965452  
#[5] SOCIALISTA DE CHILE          :  121755  
#[4] POR LA DEMOCRACIA            :  110191  
#[2] PARTIDO DEMOCRATA CRISTIANO  :  106228  
#[3] UNION DEMOCRATA INDEPENDIENTE:   99195  
#[6] COMUNISTA DE CHILE           :   83424  
#(Other)                          :  369474  

# Rango Edad     
# 30-34  :1468700  
# 25-29  :1443380  
# 35-39  :1335268  
# 45-49  :1313503  
# 20-24  :1300929  
# 50-54  :1283966  
# (Other):6709973  

#                  Region       
#Metropolitana De Santiago                :5839397  
#De Valparaiso                            :1585206  
#Del Biobio                               :1325880  
#Del Maule                                : 875685  
#De La Araucania                          : 874304  
#Del Libertador General Bernardo O'Higgins: 763106  
# (Other)                                  :3592141  
 
 #       Sexo               Sufragio      
 # femenino :7620800   no sufragó:7313660  
 # masculino:7234919   sufragó   :7542059  
                                       
#  VotoExterior       Votaron       
# Exterior:   59522   Si  :7542059  
# Nacional:14796197   NA's:7313660  


any(!complete.cases(votaciones))
map_dbl(votaciones, .f = function(x){sum(is.na(x))})

#Votaron 
# 7313660 

# Se confirma lo considerado anteriormente y variable votaron cuenta con alta cantidad de valores nulos

# Otro elemento es el correspondiente a la variable edad, donde se tiene un maximo de 138
# por ello, se procede a generar un filtro hasta 110 an~os.

votaciones <- votaciones %>% filter(Edad <= 110)


summary (votaciones)


#### Tablas de informacion ####

datasummary_skim(votaciones, type = 'categorical')



table (votaciones$Sufragio,votaciones$Sexo)                

#           femenino masculino
#no sufragó  3613900   3671863
#sufragó     3993779   3548240


proportions(table(votaciones$Sufragio,votaciones$Sexo), margin = 1)

#             femenino masculino
# no sufragó 0.4960222 0.5039778
# sufragó    0.5295371 0.4704629


proportions(table(votaciones$Sufragio,votaciones$Sexo), margin = 2)

#           femenino masculino
# no sufragó 0.4750332 0.5085610
# sufragó    0.5249668 0.4914390

table (votaciones$Sufragio,votaciones$`Rango Edad`)   
 #           18 a 19  20-24  25-29 30-34 35-39  40-44
#no sufragó  215856 559645 636213 679407 659480 622689
#sufragó     276790 741284 807167 789293 675788 617406

#            45-49  50-54  55-59  60-64  65-69  70-74
#no sufragó 649640 612176 580939 489272 403576 346931
#sufragó    663863 671790 684419 572248 425266 300992

#              75-79 80 o +
#no sufragó 300216 557620
#sufragó    184985 130768

proportions(table (votaciones$Sufragio,votaciones$`Rango Edad`), margin=1)  

#            18 a 19      20-24      25-29      30-34      35-39
#no sufragó 0.02951409 0.07652051 0.08698969 0.09289562 0.09017100
#sufragó    0.03669953 0.09828669 0.10702210 0.10465219 0.08960259

#              40-44      45-49      50-54      55-59      60-64
#no sufragó 0.08514055 0.08882557 0.08370310 0.07943205 0.06689838
#sufragó    0.08186173 0.08802145 0.08907249 0.09074697 0.07587424

#                65-69      70-74      75-79     80 o +
#no sufragó 0.05518113 0.04743603 0.04104867 0.07624363
#sufragó    0.05638593 0.03990847 0.02452712 0.01733850

proportions(table (votaciones$Sufragio,votaciones$`Rango Edad`), margin=2)

#            18 a 19     20-24     25-29     30-34     35-39
#no sufragó 0.4381564 0.4301887 0.4407800 0.4625907 0.4938934
#sufragó    0.5618436 0.5698113 0.5592200 0.5374093 0.5061066

#              40-44     45-49     50-54     55-59     60-64
#no sufragó 0.5021301 0.4945859 0.4767852 0.4591104 0.4609164
#sufragó    0.4978699 0.5054141 0.5232148 0.5408896 0.5390836

#              65-69     70-74     75-79    80 o +
#no sufragó 0.4869155 0.5354510 0.6187456 0.8100374
#sufragó    0.5130845 0.4645490 0.3812544 0.1899626


table (votaciones$Region,votaciones$Sufragio)   

#                                                no sufragó
#                                                  28586
#De Antofagasta                                   244446
#De Arica Y Parinacota                            105034
#De Atacama                                       124998
#De Aysen Del General Carlos Ibañez Del Campo      59038
#De Coquimbo                                      317878
#De La Araucania                                  523716
#De Los Lagos                                     417821
#De Los Rios                                      191140
#De Magallanes Y De La Antartica Chilena           93772
#De Ñuble                                         243573
#De Tarapaca                                      141569
#De Valparaiso                                    732685
#Del Biobio                                       692130
#Del Libertador General Bernardo O'Higgins        376937
#Del Maule                                        478499
#Metropolitana De Santiago                       2541838
                                              
#                                               sufragó
#                                                30936
#  De Antofagasta                                233272
#  De Arica Y Parinacota                          86464
#  De Atacama                                    115768
#  De Aysen Del General Carlos Ibañez Del Campo   38400
#  De Coquimbo                                   288530
#  De La Araucania                               350588
#  De Los Lagos                                  313603
#  De Los Rios                                   155705
#  De Magallanes Y De La Antartica Chilena        65846
#  De Ñuble                                      179811
#  De Tarapaca                                   115951
#  De Valparaiso                                 852521
#  Del Biobio                                    633750
#  Del Libertador General Bernardo O'Higgins     386169
#  Del Maule                                     397186
#  Metropolitana De Santiago                    3297559


votaciones %>% 
  group_by(Sufragio) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE))

#   Sufragio   edad_prom
#    <fct>          <dbl>
#  1 no sufragó      48.7
#  2 sufragó         44.4

votaciones %>% 
  filter(Sufragio=='sufragó') %>% 
  group_by(Sexo) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE))

#     Sexo      edad_prom
#      <fct>         <dbl>
#  1 femenino       44.5
#  2 masculino      44.4


votaciones %>% 
  filter(Sufragio=='no sufragó') %>% 
  group_by(Sexo) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE))

#     Sexo       edad_prom
#    <fct>         <dbl>
#  1 femenino       50.5
#  2 masculino      47.0

tabla_votaciones <- votaciones %>% 
  group_by(Sexo,Sufragio) %>%
  summarise('Edad promedio'= mean(Edad, na.rm=TRUE))

opciones_forma <- c('striped', "bordered", 'hover', 'condensed', 'responsive')

knitr::kable(tabla_votaciones, booktabs = TRUE, caption = 'Edad Promedio de Sufragio por Genero') %>% 
  kable_styling(bootstrap_options = opciones_forma, full_width = FALSE, font_size = 12)

votaciones %>% 
  filter(Sufragio=='sufragó') %>% 
  group_by(Region) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE)) %>% 
  arrange(desc(edad_prom))

#      Region                                         edad_prom
#      <fct>                                              <dbl>
#  1 "De Ñuble"                                           45.4
#  2 "De Valparaiso"                                      45.4
#  3 "De Los Rios"                                        45.2
#  4 "De Arica Y Parinacota"                              45.2
#  5 "Del Maule"                                          44.9
#  6 "Del Biobio"                                         44.8
#  7 "De La Araucania"                                    44.8
#  8 "De Coquimbo"                                        44.7
#  9 ""                                                   44.6
#  10 "Del Libertador General Bernardo O'Higgins"         44.5
#  11 "De Atacama"                                        44.2
#  12 "Metropolitana De Santiago"                         44.1
#  13 "De Magallanes Y De La Antartica Chilena"           44.0
#  14 "De Aysen Del General Carlos Ibañez Del Campo"      44.0
#  15 "De Los Lagos"                                      44.0
#  16 "De Tarapaca"                                       43.7
#  17 "De Antofagasta"                                    43.2
  
votaciones %>% 
  filter(Sufragio=='no sufragó') %>% 
  group_by(Region) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE)) %>% 
  arrange(desc(edad_prom)) 


#      Region                                        edad_prom
#     <fct>                                              <dbl>
#  1 "De Magallanes Y De La Antartica Chilena"          50.7
#2 ""                                                   50.4
#3 "De Valparaiso"                                      50.1
#4 "De La Araucania"                                    50.0
#5 "De Los Rios"                                        49.7
#6 "De Ñuble"                                           49.3
#7 "De Los Lagos"                                       48.9
#8 "De Aysen Del General Carlos Ibañez Del Campo"       48.9
#9 "Del Libertador General Bernardo O'Higgins"          48.8
#10 "Del Maule"                                         48.7
#11 "Metropolitana De Santiago"                         48.6
#12 "Del Biobio"                                        48.4
#13 "De Coquimbo"                                       47.8
#14 "De Atacama"                                        46.5
#15 "De Arica Y Parinacota"                             46.5
#16 "De Tarapaca"                                       46.5
#17 "De Antofagasta"                                    45.0

# Edad promedio de votantes por region y genero (sexo)

region_edad <-votaciones_ne %>% 
  filter(Sufragio=='sufragó') %>% 
  group_by(Region,Sexo) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE)) %>% 
  arrange(desc(edad_prom))

opciones_forma <- c('striped', "bordered", 'hover', 'condensed', 'responsive')

knitr::kable(region_edad, booktabs = TRUE, caption = 'Edad Promedio de Sufragio por Genero') %>% 
  kable_styling(bootstrap_options = opciones_forma, full_width = FALSE, font_size = 12)

# Edad promedio de los que no votaron por region y genero (sexo)

region_edad_no <-votaciones_ne %>% 
  filter(Sufragio=='no sufragó') %>% 
  group_by(Region,Sexo) %>%
  summarise(edad_prom= mean(Edad, na.rm=TRUE)) %>% 
  arrange(desc(edad_prom))

opciones_forma <- c('striped', "bordered", 'hover', 'condensed', 'responsive')

knitr::kable(region_edad_no, booktabs = TRUE, caption = 'Edad Promedio de Sufragio por Genero') %>% 
  kable_styling(bootstrap_options = opciones_forma, full_width = FALSE, font_size = 12)

#### Graficos de analisis ####

# Sufragio por Edad # 

voto_edad <- proportions(table (votaciones$Sufragio,votaciones$`Rango Edad`), margin=2)%>%
  round(digits = 3)

barplot(voto_edad, 
        beside = TRUE, las=1, 
        main = "Porcentaje de votación por grupo Etario",
        xlab='Tramo Etario', ylab='Frecuencia relativa',
        col = c("lightblue", "mistyrose"),
        ylim = c(0, 1))
legend('topleft', legend=rownames(voto_edad), bty='n',
       fill=c("lightblue", "mistyrose"))

library(flextable)

# Tabla Participacion por Region

votaciones_ne <- votaciones%>% filter(Region != "")

votaciones_ne <- as.data.frame(votaciones_ne)

 voto_reg_1 <-  proportions(table (votaciones_ne$Region,votaciones_ne$Sufragio), margin=2)%>%
    round(digits = 3) 
 opciones_forma <- c('striped', "bordered", 'hover', 'condensed', 'responsive')
 
 knitr::kable(voto_reg_1 , booktabs = TRUE, caption = 'Porcentaje de votantes por Region') %>% 
   kable_styling(bootstrap_options = opciones_forma, full_width = FALSE, font_size = 12)
 
 # Grafico de participacion por region
 
 voto_reg_2 <- proportions(table (votaciones_ne$Sufragio,votaciones_ne$Region), margin=2)%>%
   round(digits = 3) 
 
 barplot(voto_reg_2, 
         beside = TRUE, las=1, 
         main = "Porcentaje de votantes por Región en Plebiscito",
         xlab='Región', ylab='Frecuencia relativa',
         col = c("lightblue", "mistyrose"),
         ylim = c(0, 1))
 legend('topleft', legend=rownames(voto_reg_2), bty='n',
        fill=c("lightblue", "mistyrose"))

 # Promedio de edad por region 
 
region_edad <- votaciones_ne %>% 
   filter(Sufragio=='sufragó') %>% 
   group_by(Region,Sexo) %>%
   summarise(edad_prom= mean(Edad, na.rm=TRUE)) %>% 
   ggplot(aes(x = Sexo, y = Cantidad, fill = Region)) + 
   geom_bar(stat="identity", position="dodge") + 
   labs(title = "Titulo")


#Caso Magallanes

maga <- votaciones_ne %>% 
  filter(Sexo=="femenino" & Region=="De Magallanes Y De La Antartica Chilena")


table(maga$Sufragio, maga$`Rango Edad`)


#           18 a 19 20-24 25-29 30-34 35-39 40-44 45-49 50-54 55-59 60-64
#no sufragó     900  2286  2729  3160  3464  3296  3487  3375  3862  3563
#sufragó       1262  3168  3437  3835  3310  3184  3065  2990  3151  2545

#             65-69 70-74 75-79 80 o +
#no sufragó  3091  2711  2353   4554
#sufragó     1624   972   585    390

table(maga$Sufragio)

#no sufragó    sufragó 
#42831      33518 

prop.table(table(maga$Sufragio))
#no sufragó    sufragó 
#0.5609897  0.4390103 

grup <- ggplot(data = maga, aes(x = `Rango Edad`, y = ..count.., fill = Sufragio)) +
  geom_bar() +
  scale_fill_manual(values = c("#9BBB5C", "#7C5CBB")) +
  labs(title = "Tramo Etario Magallanes Femenino") +
  theme_bw() +
  theme(legend.position = "bottom")

grup

mujeres<- votaciones_ne %>% 
  filter(Sexo=="femenino")

table(mujeres$Region,mujeres$Sufragio)

prop.table(table(mujeres$Region,mujeres$Sufragio), margin = 2)


prop.table(table(mujeres$Sufragio))

# Para profundizar en el componente geografico se genera una nueva variable
#haciendo consideracion a nivel de zonas del pais

zonaNorte = c(
  "De Arica Y Parinacota",
  "De Tarapaca",
  "De Antofagasta",
  "De Atacama",
  "De Coquimbo")

zonaCentro = c(
  "De Valparaiso",
  "Metropolitana De Santiago",
  "Del Libertador General Bernardo O'Higgins",
  "Del Maule",
  "De Ñuble",
  "Del Biobio")

zonaSur = c(
  "De La Araucania",
  "De Los Rios",
  "De Los Lagos",
  "De Aysen Del General Carlos Ibañez Del Campo", 
  "De Magallanes Y De La Antartica Chilena")

votaciones_ne <- votaciones_ne %>% 
  mutate(ZonaPais = ifelse(Region %in% zonaNorte, "Norte", ifelse(Region %in% zonaCentro, "Centro", "Sur")))

votaciones_ne$ZonaPais <- as.factor(votaciones_ne$ZonaPais )

table(votaciones_ne$Sufragio, votaciones_ne$ZonaPais)

prop.table(table(votaciones_ne$Sufragio, votaciones_ne$ZonaPais), margin=2)

grup_2 <- ggplot(data = votaciones_ne, aes(x = ZonaPais, y = ..count.., fill = Sufragio)) +
  geom_bar() +
  scale_fill_manual(values = c("darkblue", "darkgreen")) +
  labs(title = "Participación por Zona del Pais") +
  scale_y_continuous(breaks=seq(300000, 11000000, 3000000))+ 
  theme_light() +
  theme(legend.position = "bottom")

grup_2

# Representacion de nivel de participacion a nivel de comuna

co <- prop.table(table(votaciones_ne$Comuna, votaciones_ne$Sufragio), margin=1)

co <- as.data.frame(co)
colnames(co)<- c('Comuna', 'Sufra', 'Votac')

co %>% 
  filter(Sufra == 'sufragó') %>% 
  group_by(Comuna) %>% 
  summarise(Suma=sum(Votac)) %>% 
  arrange(desc(Suma))

# Representacion del nivel de abstencion

co %>% 
  filter(Sufra == 'no sufragó') %>% 
  group_by(Comuna) %>% 
  summarise(Suma=sum(Votac)) %>% 
  arrange(desc(Suma))

co %>% 
  filter(Sufra == 'no sufragó') %>% 
  group_by(Comuna) %>% 
  summarise(Suma=sum(Votac)) %>% 
  arrange(desc(Suma))


#### Tratamiento de bases para inferencias ####

votaciones_pre <- votaciones_ne %>% 
  select(-Cedula, -DV, -Votaron)

# Se "damiza" la variable Sexo (genero) y Sufragio#

votaciones_pre<- votaciones_pre %>% 
  mutate(Genero = ifelse(Sexo == "femenino", 1, 0), Voto = ifelse(Sufragio == "sufragó", 1, 0)) 

# se genera tabla de resumen de votacion total por comuna y se pasa a data frame
sufra_comuna <- table(votaciones_pre$Comuna,votaciones_pre$Sufragio )
sufra_comuna <- as.data.frame(sufra_comuna)
colnames(sufra_comuna)<- c('Comuna', 'Sufra', 'Votac')

# se genera tabla de resumen de votacion femenina por comuna y se pasa a data frame

voto_mujeres<- votaciones_ne %>% 
  filter(Sufragio == 'sufragó' & Sexo =='femenino') %>% 
  group_by(Comuna) %>% 
  summarise(Voto_fem=sum(Votaron)) 

# se genera tabla de resumen de votacion masculina por comuna y se pasa a data frame

voto_hom <- votaciones_ne %>% 
  filter(Sufragio == 'sufragó' & Sexo =='masculino') %>% 
  group_by(Comuna) %>% 
  summarise(Voto_masc=sum(Votaron)) 

votos_agru <- voto_mujeres %>% inner_join(voto_hom, by="Comuna")

# se genera data con comuna y promedio de edad
voto_edad<- votaciones_ne %>% 
  filter(Sufragio == 'sufragó')%>% 
  group_by(Comuna) %>% 
  summarise(promediom=sum(Edad)) 

votos_agru_edad <- votos_agru %>% inner_join(voto_edad, by="Comuna")

#Dado a que el nivel de participacion esta en relacion con el N que sufrago,
# se selecciona esta variable, algunos lo pudieran discutir y sen~alar que debiese verse 
# en valores porcentuales

si_sufra_comuna <- sufra_comuna %>% 
  filter(Sufra=='sufragó')

votos <- votos_agru_edad %>% inner_join(si_sufra_comuna, by="Comuna")

votos <- votos %>% 
  select(-Sufra)

votos <- votos %>% 
  rename(Promedio_edad=Promedio, Votos_total=Votac)


# Se integra la base con el numero de votos de Apruebo y niveles de pobreza

library(readxl)
plebiscito <- read_excel("Resultados Plebiscito Constitucion Politica 2020.xlsx", sheet = 'Comuna')

glimpse(plebiscito)

base <- plebiscito %>% 
  select(Comuna,APRUEBO, 
         `Número de personas en situación de pobreza multidimensional`,
         `Número de personas en situación de pobreza por ingresos`) %>% 
  rename(Comuna=Comuna, Apruebo= APRUEBO, Multidimensional=`Número de personas en situación de pobreza multidimensional`,
         Ingreso=`Número de personas en situación de pobreza por ingresos`)

# Posterior a realizar conversiones de nombre para su mejor entendimiento
# se hace un inner join a las bases


votaciones_predict <- votos %>%  inner_join(base, by = 'Comuna')

votaciones_predict$Comuna <- as.factor(votaciones_predict$Comuna)

# Dejamos a la comuna como variable de observacion

regre <- data.frame(votaciones_predict[,-1], row.names = votaciones_predict$Comuna)

# Revisamos eventuales correlaciones

library(GGally)
ggpairs(regre, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Se genera el modelo

modelo <- lm(Votos_total ~ Multidimensional + Ingreso + Promedio_edad, data = regre)
summary(modelo)

# se seleccionan los mejores predictores

step(object = modelo, direction = "both", trace = 1)


#Aprendizaje

# Se profundiza la necesidad de generar un proceso por etapas y pasos que permitan
# analizar las variables incorporando elementos adicionales, asi como,
# la necesidad de mejorar el uso de elementos visuales que faciliten la comprension
# de la infromacion que se presenta. 
# Con mayor nivel de conocimientos, se debio o escalar las dimensiones dado su diversa magnitud 
