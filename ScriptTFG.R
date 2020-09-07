library(dplyr)
library(ggplot2)



#LIMPIEZA DE DATOS

              #dataset TIRO NBA NCAA

#Organizamos columnas en su sitio
p<-names(playersBuenoconID)[1:3]
s<-names(playersBuenoconID)[5:34]
names(playersBuenoconID)<-c(names(playersBuenoconID)[1:4], "year",names(playersBuenoconID)[5:34])
playersbueno2<-playersBuenoconID
#Cogemos solo jugadores en activo desde 1980, y que hayan jugado en una sóla universidad o venga desde instituto o de otro país
playersbueno2 <- playersbueno2[playersbueno2$active_from > 1980, ]
playersbueno2<-playersbueno2[playersbueno2$height < "7-7", ]
playersbueno2$college[is.na(playersbueno2$college)]<-"High School/International"

#Preparación datos NCAA NBA TIRO
playersbueno2$NCAA_efgpct[is.na(playersbueno2$NCAA_efgpct)]<-0
playersbueno2$NCAA__3ptapg[is.na(playersbueno2$NCAA__3ptapg)]<-0
playersbueno2$NCAA__3ptpct[is.na(playersbueno2$NCAA__3ptpct)]<-0
playersbueno2$NCAA__3ptpg[is.na(playersbueno2$NCAA__3ptpg)]<-0
playersbueno2$NCAA_fgapg[is.na(playersbueno2$NCAA_fgapg)]<-0
playersbueno2$NCAA_fgpct[is.na(playersbueno2$NCAA_fgpct)]<-0
playersbueno2$NCAA_fgpg[is.na(playersbueno2$NCAA_fgpg)]<-0

shootingNCAA_NBA<-playersBueno

#Creamos datsets específicos...

#Jugadores con más de 50 partidos en NBA (para poder quitarnos muchos NA en stats de NBA)
NBAPlayersMT50Games<-playersbueno2
NBAPlayersMT50Games<-NBAPlayersMT50Games[NBAPlayersMT50Games$NBA_g_played >50, ]

#Por posición:
#Centers
NBAPlayersMT50Centers<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'C', ]
#Powerforwards y forwards se pueden fusionar
#Power Forwards
NBAPlayersMT50FC<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'F-C', ]
#Forwards
NBAPlayersMT50F<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'F', ]
#Small Forward
NBAPlayersMT50FG<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'F-G', ]
#Point Guards
NBAPlayersMT50GF<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'G-F', ]
#Guards
NBAPlayersMT50G<-NBAPlayersMT50Games[NBAPlayersMT50Games$position == 'G', ]

#Eliminamos columnas innecesarias
NBAPlayersMT50Games$id<-NULL



                #Dataset NCAAIdeales
NCAAIdeales<-select(NCAAIdeales, Name, everything())
NCAAIdeales$SOS<-NULL
NCAAIdeales$X28<-NULL
NCAAIdeales$X32<-NULL
NCAAIdeales$Season<-NULL
NCAAIdeales$Conf<-NULL

                #SEASON_STATS
Seasons_Stats <- Seasons_Stats[Seasons_Stats$Year > 1980, ]
deleteSeasonStats<-c("GS", "3PAr", "ORB%", "DRB%", "TRB%", "AST%", "STL%", "BLK%", "TOV%", "USG%", "blanl", "OWS", "DWS", "WS", "WS/48", "blank2", "OBPM", "DBPM", "BPM", "VORP", "3P", "3PA", "3P%", "ORB", "DRB", "STL", "BLK")
Seasons_Stats<-Seasons_Stats[, !(names(Seasons_Stats) %in% deleteSeasonStats)]
Seasons_Stats$TOV<-NULL
Seasons_Stats$X1[is.na(Seasons_Stats$X1)]<-0
Seasons_Stats<-Seasons_Stats[Seasons_Stats$X1>1, ]
Seasons_Stats$X1<-NULL




#Prueba caras de chernoff

install.packages("aplpack")
library(aplpack)

#Para guardar en csv:
 write.csv2(SeasonStats2018,"seasonStats2018_1Year.csv",)


#Chernoff

#Ejemplo 1 2009

#Draft Combine
Data<-nba_draft_combine_all_years
nba_draft_combine_2009<-Data[Data$Year == 2009, ]
nba_draft_combine_2009<-nba_draft_combine_2009[with(nba_draft_combine_2009, order(nba_draft_combine_2009$`Draft pick`)), ]
faces2009_1_Dim<-faces(as.matrix(nba_draft_combine_2009[1:20, 6:8]), main = "NBA 2009 (Height, Wingspan, Standing reach)", labels=nba_draft_combine_2009$Player[1:20])
faces2009_1_jump<-faces(as.matrix(nba_draft_combine_2009[1:20, 9:12]), main = "NBA 2009 (Vertical Tryouts)", labels=nba_draft_combine_2009$Player[1:20])
faces2009_1_weight<-faces(as.matrix(nba_draft_combine_2009[1:20, 13:14]), main = "NBA 2009 (Weight and Body Fat)", labels=nba_draft_combine_2009$Player[1:20])
faces2009_1_tests<-faces(as.matrix(nba_draft_combine_2009[1:20, 17:19]), main = "NBA 2009 (Bench, agility, sprint)", labels=nba_draft_combine_2009$Player[1:20])

#Shooting dif
#ordenamos  y arreglamos el dataset
names(shootingNCAA_NBA)[4]='Player'
shootingNCAA_NBA$weight<-as.numeric(shootingNCAA_NBA$weight)
shootingNCAA_NBA$NBA__3ptapg<-as.numeric(shootingNCAA_NBA$NBA__3ptapg)
shootingNCAA_NBA$id<-NULL
shootingNCAA_NBA$active_to<-NULL
shootingNCAA_NBA$college<-NULL
shootingNCAA_NBA$birth_date<-NULL
shootingNCAA_NBA$NBA__3ptpct<-NULL
shootingNCAA_NBA$NBA_efgpct<-NULL
shootingNCAA_NBA$NCAA__3ptpct<-NULL
shootingNCAA_NBA$NCAA_efgpct<-NULL
shootingNCAA_NBA$NCAA_fgpct<-NULL

#Somos precisos con la temporada 2010
Seasons_Stats_2010<-Seasons_Stats[Seasons_Stats$Year==2010, ]
Seasons_Stats_2010_Guards<-Seasons_Stats_2010[Seasons_Stats_2010$Pos=='G', ]
Seasons_Stats_2010_SG<-Seasons_Stats_2010[Seasons_Stats_2010$Pos == 'SG', ]
#unimos ambos
Seasons_Stats_2010_GSG<-rbind(Seasons_Stats_2010_Guards,Seasons_Stats_2010_SG)
#filtramos por edad
Seasons_Stats_2010_GSG_1Year<-Seasons_Stats_2010_GSG[Seasons_Stats_2010_GSG$Age<23, ]
#eliminamos NAs
Seasons_Stats_2010_GSG_1Year[is.na(Seasons_Stats_2010_GSG_1Year)]<-0


#Shooting diference NCAA/1ºYearNBA/NBACarreer
shootingNCAA_NBA_2009<-shootingNCAA_NBA[shootingNCAA_NBA$active_from == 2010, ]
shootingNCAA_NBA_2009_Guards<-shootingNCAA_NBA_2009[shootingNCAA_NBA_2009$position == 'G', ]
Seasons_Stats_2010_GSG_1Year<-merge(Seasons_Stats_2010_GSG_1Year,shootingNCAA_NBA_2009_Guards, by = 'Player')
Seasons_Stats_2010_GSG_1Year$AVGPTS=Seasons_Stats_2010_GSG_1Year$PTS/Seasons_Stats_2010_GSG_1Year$G
Seasons_Stats_2010_GSG_1Year$NCAA_FG = Seasons_Stats_2010_GSG_1Year$NCAA_fgpg/Seasons_Stats_2010_GSG_1Year$NCAA_fgapg
faces2009_1_Weight_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 7:7]), main = "NBA 2009 Guards (Weight)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_NBASHOOT_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 8:15]), main = "NBA 2009 Guards (NBA Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_NCAASHOOT_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 18:24]), main = "NBA 2009 Guards (NCAA Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_3PNBA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 8:9]), main = "NBA 2009 Guards (NBA 3 Point Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_3PNCAAA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 18:19]), main = "NBA 2009 Guards (NCAA 3 Point Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_FGNBA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 11:12]), main = "NBA 2009 Guards (NBA FG Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_FGNCAAA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 20:21]), main = "NBA 2009 Guards (NCAA FG Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_FTNBA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 14:14]), main = "NBA 2009 Guards (NBA FT Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_FTNCAAA_Guards<-faces(as.matrix(shootingNCAA_NBA_2009_Guards[1:23, 23:24]), main = "NBA 2009 Guards (NCAA FT Shooting)", labels=shootingNCAA_NBA_2009_Guards$Player[1:23])
faces2009_1_FTNBA1Year_Guards<-faces(as.matrix(Seasons_Stats_2010_GSG_1Year[1:18, 20:20]), main = "NBA 2009 Guards (NBA FT 1 Year Shooting)", labels=Seasons_Stats_2010_GSG_1Year$Player[1:18])

faces2009_1_FGNCAA_GuardsPT<-faces(as.matrix(Seasons_Stats_2010_GSG_1Year[1:18, 49:49]), main = "NBA 2009 Guards (NCAA FG %)", labels=Seasons_Stats_2010_GSG_1Year$Player[1:18])
faces2009_1_FGNBA1Year_GuardsPT<-faces(as.matrix(Seasons_Stats_2010_GSG_1Year[1:18, 13:13]), main = "NBA 2009 Guards (NBA 1 Year FG %)", labels=Seasons_Stats_2010_GSG_1Year$Player[1:18])

faces2009_1_PTSNBA1Year_Guards<-faces(as.matrix(Seasons_Stats_2010_GSG_1Year[1:18, 50:50]), main = "NBA 2009 Guards (NBA AVG Points 1 Year)", labels=Seasons_Stats_2010_GSG_1Year$Player[1:18])
faces2009_1_PTSNBA_Guards<-faces(as.matrix(Seasons_Stats_2010_GSG_1Year[1:18, 40:40]), main = "NBA 2009 Guards (NBA AVG Points)", labels=Seasons_Stats_2010_GSG_1Year$Player[1:18])

#Uso de algortimos de clustering(2009)
dataStudy<-Seasons_Stats_2010_GSG_1Year
#Al haber jugado en equipos distintos en su primer años, eliminamos a los jugadores determinados
dataStudy<-dataStudy[dataStudy$Player!="Eric Maynor", ]
dataStudy<-dataStudy[dataStudy$Player!="Jodie Meeks", ]
#Escalar valores de tiro (FG%&FT%) de: Carrera NBA, NCAA, First Year NBA
dataStudy.scale<-as.data.frame(scale(dataStudy[,c(13, 20,33, 36, 45, 49)]))

#Obtenemos la escalada con nombres
row.names(dataStudy.scale)<-dataStudy$Player
row.names(dataStudy)<-dataStudy$Player

######
###Custering with dendograms, hclustering applyed to the same 
# group of players, with a different selection of variables, here 3:10
######
#Hacemos varios result con:
#Porcentajes de tiro combinados (FG% & FT%)
result<-hclust(dist(dataStudy[,c(13, 20,33, 36, 45, 49)],method="euclidian"),method="single")
#solo %FG
result<-hclust(dist(dataStudy[,c(13,33,49)],method="euclidian"),method="single")
#Solo %FT
result<-hclust(dist(dataStudy[,c(20, 36, 45)],method="euclidian"),method="single")

#Siendo más específicos, vamos a ver qué ocurría en NCAA:
#FG%
result<-hclust(dist(dataStudy[,c(43:44,49)],method="euclidian"),method="single")
#FT%
result<-hclust(dist(dataStudy[, 45:47],method="euclidian"),method="single")

plot(result)
help("plot")

#Empezamos con KMeans
set.seed(123)

sumbts<-kmeans(dataStudy.scale, centers = 1)$betweenss
sumtws<-kmeans(dataStudy.scale, centers = 1)$tot.withinss

for (i in 2:11) {
  sumbts[i]<-kmeans(dataStudy.scale, centers = i)$betweenss
  sumtws[i]<-kmeans(dataStudy.scale, centers = i)$tot.withinss
  
}
#Obtenemos la representación gráfica del número de clusters.
plot(1:11, sumbts, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")
plot(1:11, sumtws, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")
#El resultado común es 7

## First try with 7 centers, later better make same with 5 or another number
datos.kmeans<-kmeans(dataStudy.scale, centers = 7) #k=centers
#observamos los resultados y la calidad de los mismos
datos.kmeans
str(datos.kmeans)
#INERCIA ENTRE GRUPOS:  mayor es mejor
datos.kmeans$betweenss
#INERCIA INTRA GRUPOS:  menor es mejor
datos.kmeans$withinss
#INERCIA TOTAL INTRA GRUPOS:  menor es mejor
datos.kmeans$tot.withinss

#representamos los resultados
playersnames<-row.names(dataStudy.scale)
grupo<-datos.kmeans$cluster
datosc<-data.frame(playersnames, grupo)
library(ggplot2)
ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=playersnames),color=grupo, size=2)



#Vamos a fusionar la tabla de la clase del 2010 en su primer año con la del 2018
#Primero preparamos la completa de 2018
FullShooting2018rookies<-merge(shootingNCAA_NBA2018,seasonStats2018_1Year, by = 'Player')
#Ahora la tabla con todo el tiro de los de 2009
Seasons_Stats_2010_1Year<-Seasons_Stats_2010[Seasons_Stats_2010$Age<23,]
FullShooting2010rookies<-merge(shootingNCAA_NBA_2009,Seasons_Stats_2010_1Year, by ='Player')

#Ahora la juntamos con la de 2009, filtrando por solo estadísticas de tiro
set1<-FullShooting2018rookies[,c(1,12,14,16:19,24,26,28,30:33,44,45)]
set2<-FullShooting2010rookies[,c(1,8:13,18:24,36:37)]
#Tenemos el dataset de tiro completo de las clases 2017 y 2009
FullShooting_2009_2018<-cbind(set1, set2)
#Arreglos adicionales..
FullShooting_2009_2018<-FullShooting_2009_2018[FullShooting_2009_2018$Player!="Eric Maynor", ]
FullShooting_2009_2018<-FullShooting_2009_2018[FullShooting_2009_2018$Player!="Jodie Meeks", ]
FullShooting_2009_2018<-FullShooting_2009_2018[FullShooting_2009_2018$Player!="Jordan Hill", ]
FullShooting_2009_2018[is.na(FullShooting_2009_2018)]<-0
FullShooting_2009_2018$NCAA_FG<-FullShooting_2009_2018$NCAA_fgpg/FullShooting_2009_2018$NCAA_fgapg
FullShooting_2009_2018$NCAA_3P<-FullShooting_2009_2018$NCAA__3ptpg/FullShooting_2009_2018$NCAA__3ptapg
FullShooting_2009_2018$NBA__3ptapg<-as.numeric(FullShooting_2009_2018$NBA__3ptapg)
FullShooting_2009_2018$NBA_3P<-FullShooting_2009_2018$NBA__3ptpg/FullShooting_2009_2018$NBA__3ptapg

#vamos a visualizar
#dendrograms:

set<-FullShooting_2009_2018
row.names(set)<-set$Player
#NCAA Overall Shooting:
result<-hclust(dist(set[,c(8:14,17:18)],method="euclidian"),method="single")
plot(result)
#NBA Overall Shooting(Only with % and Attempts)
result<-hclust(dist(set[,c(2,4,6:7,15:16,19)],method="euclidian"),method="single")
plot(result)
#NBA Only with %
result<-hclust(dist(set[,c(4,7,16,19)],method="euclidian"),method="single")
plot(result)

#KMeans:
#creamos escala:
setNBA.scale<-as.data.frame(scale(set[,c(2:7,19)]))
setNCAA.scale<-as.data.frame(scale(set[,c(8:12,17:18)]))
row.names(setNBA.scale)<-set$Player
row.names(setNCAA.scale)<-set$Player
#Empezamos
set.seed(123)
          #Primero NBA
sumbts<-kmeans(setNBA.scale, centers = 1)$betweenss
sumtws<-kmeans(setNBA.scale, centers = 1)$tot.withinss

for (i in 2:25) {
  sumbts[i]<-kmeans(setNBA.scale, centers = i)$betweenss
  sumtws[i]<-kmeans(setNBA.scale, centers = i)$tot.withinss
  
}
#Obtenemos la representación gráfica del número de clusters.
plot(1:25, sumbts, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")
plot(1:25, sumtws, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")


# First try with 13 centers
datos.kmeans<-kmeans(setNBA.scale, centers = 13) #k=centers
#observamos los resultados y la calidad de los mismos
datos.kmeans
str(datos.kmeans)
#INERCIA ENTRE GRUPOS:  mayor es mejor
datos.kmeans$betweenss
#INERCIA INTRA GRUPOS:  menor es mejor
datos.kmeans$withinss
#INERCIA TOTAL INTRA GRUPOS:  menor es mejor
datos.kmeans$tot.withinss

#representamos los resultados
playersnames<-row.names(setNBA.scale)
grupo<-datos.kmeans$cluster
datosc<-data.frame(playersnames, grupo)
library(ggplot2)
ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=playersnames),color=grupo, size=2)


          #AHORA NCAA
sumbtsNCAA<-kmeans(setNCAA.scale, centers = 1)$betweenss
sumtwsNCAA<-kmeans(setNCAA.scale, centers = 1)$tot.withinss

for (i in 2:25) {
  sumbtsNCAA[i]<-kmeans(setNCAA.scale, centers = i)$betweenss
  sumtwsNCAA[i]<-kmeans(setNCAA.scale, centers = i)$tot.withinss
  
}
#Obtenemos la representación gráfica del número de clusters.
plot(1:25, sumbts, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")
plot(1:25, sumtws, type="b", xlab = "number of clusters", ylab = "betweens sum of the squares")


# First try with 13 centers
datosNCAA.kmeans<-kmeans(setNBA.scale, centers = 13) #k=centers
#observamos los resultados y la calidad de los mismos
datosNCAA.kmeans
str(datosNCAA.kmeans)
#INERCIA ENTRE GRUPOS:  mayor es mejor
datosNCAA.kmeans$betweenss
#INERCIA INTRA GRUPOS:  menor es mejor
datosNCAA.kmeans$withinss
#INERCIA TOTAL INTRA GRUPOS:  menor es mejor
datosNCAA.kmeans$tot.withinss

#representamos los resultados
playersnames<-row.names(setNCAA.scale)
grupo<-datosNCAA.kmeans$cluster
datosc<-data.frame(playersnames, grupo)
library(ggplot2)
ggplot(datosc)+
  geom_point(mapping=aes(x=grupo, y=playersnames),color=grupo, size=2)