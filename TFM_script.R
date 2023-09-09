setwd("C:/Users/charl/Documents/CloudStation/Uni/Master/TFM")

library(dummies)
library(ggplot2)
library(skimr)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(caret)

spotify_train <- read_csv("spotify_train.csv")
spotify_test <- read.csv("spotify_test.csv")

spotify_train2 <- spotify_train |> 
  mutate(role = "train")

spotify_test2 <- spotify_test |> 
  mutate(role = "test") |> 
  select(-c("score","track_uri"))
spotify_score <- spotify_test |>
  mutate(id = 1959 + row_number()) |> 
  select(c("id", "title", "artist", "score"))

spotify_raw <- rbind(spotify_train2, spotify_test2)
spotify_raw2 <- as.data.frame(spotify_raw)

library(foreign)
write.dbf(spotify_raw2, "C:/Users/charl/Documents/CloudStation/Uni/Master/TFM/spotifyraw.dbf")

# Comprobamos que no hay ninguna canción de un mismo artista que se repita
spotify_raw |> count(title, artist) |> filter(n>1)
# Date mal codififcada
spotify_raw |> select(date)

spotify <- spotify_raw |> 
  mutate(duration_min = duration_ms/60000,
         fade_out = duration_ms/1000 - fade_out,
         explicit = case_when(str_detect(explicit, "FALSE") ~ 0,
                              str_detect(explicit, "False") ~ 0,
                              str_detect(explicit, "TRUE") ~ 1,
                              str_detect(explicit, "True") ~ 1),
         collab = if_else(num_artists==1, 0, 1),
         id = row_number()) |>
  rename(major = mode) |> 
  separate(date, into = c("year", "month-day"), sep = "-", convert = TRUE) |>
  select(-c("title", "artist","num_artists", "month-day", "duration_ms")) |> 
  mutate(fade_out = if_else(fade_out < 0, 0, fade_out)) |> 
  select(-c("instrumentalness", "time_signature")) |> 
  mutate(fade_out = if_else(fade_out>60, NA, fade_out),
         fade_in = if_else(fade_in>10, NA, fade_in),
         fade_out = replace_na(fade_out, median(fade_out, na.rm = TRUE)),
         fade_in = replace_na(fade_in, median(fade_in, na.rm = TRUE))) |> 
  mutate(genre = case_when(str_detect(genre, "indie") ~ "indie",
                           str_detect(genre, "hip hop") ~ "hiphop",
                           str_detect(genre, "rock") ~ "rock",
                           str_detect(genre, "trap") ~ "hiphop",
                           str_detect(genre, "rap") ~ "rap",
                           str_detect(genre, "edm") ~ "edm",
                           str_detect(genre, "pop") ~ "pop",
                           str_detect(genre, "r&b") ~ "rb",
                           str_detect(genre, "soul") ~ "rb",
                           str_detect(genre, "jazz") ~ "rb",
                           str_detect(genre, "electro") ~ "edm",
                           str_detect(genre, "dance") ~ "edm",
                           str_detect(genre, "house") ~ "edm",
                           str_detect(genre, "techno") ~ "edm",
                           str_detect(genre, "reggaeton") ~ "hiphop",
                           str_detect(genre, "urban") ~ "hiphop",
                           str_detect(genre, "reggae") ~ "folk",
                           str_detect(genre, "flamenco") ~ "folk",
                           str_detect(genre, "country") ~ "folk",
                           str_detect(genre, "folk") ~ "folk",
                           str_detect(genre, "punk") ~ "rock",
                           str_detect(genre, "metal") ~ "rock",
                           str_detect(genre, "cantautor") ~ "cantautor",
                           genre == "alt z" ~ "rock",
                           str_detect(genre, "boom bap") ~ "hiphop",
                           genre == "big room" ~ "edm",
                           genre == "francoton" ~ "hiphop",
                           genre == "latin talent show" ~ "others",
                           genre == "neo mellow" ~ "rock",
                           genre == "boy band" ~ "pop",
                           genre == "girl group" ~ "pop",
                           genre == "drill espanol" ~ "hiphop",
                           genre == "brostep" ~ "edm",
                           str_detect(genre, "hardstyle") ~ "edm",
                           genre == "carnaval cadiz" ~ "folk",
                           genre == "adult standards" ~ "others",
                           genre == "frenchcore" ~ "edm",
                           genre == "nu gabber" ~ "edm",
                           genre == "permanent wave" ~ "rock",
                           str_detect(genre, "bass") ~ "edm",
                           genre == "rumba catalana" ~ "folk",
                           str_detect(genre, "ska") ~ "rock",
                           genre == "cancion melodica" ~ "cantautor",
                           genre == "dembow" ~ "hiphop",
                           genre == "perreo" ~ "hiphop",
                           genre == "disco" ~ "edm",
                           genre == "mestissatge" ~ "rock",
                           genre == "hollywood" ~ "others",
                           genre == "broadway" ~ "others",
                           genre == "movie tunes" ~ "others",
                           genre == "anime" ~ "others",
                           genre == "cartoon" ~ "others",
                           genre == "comedie musicale" ~ "others",
                           genre == "latin alternative" ~ "rock",
                           str_detect(genre, "oi") ~ "rock",
                           genre == "salsa" ~ "folk",
                           genre == "cuarteto" ~ "folk",
                           str_detect(genre, "cumbia") ~ "folk",
                           genre == "bachata" ~ "folk",
                           str_detect(genre, "bolero") ~ "folk",
                           genre == "cha-cha-cha" ~ "folk",
                           genre == "corrido" ~ "folk",
                           genre == "mambo" ~ "folk",
                           genre == "merengue" ~ "folk",
                           genre == "strut" ~ "pop",
                           str_detect(genre, "funk") ~ "hiphop",
                           genre == "chillwave" ~ "edm",
                           str_detect(genre, "new wave") ~ "rock",
                           genre == "afrofuturism" ~ "rb",
                           genre == "afroswing" ~ "hiphop",
                           str_detect(genre, "afrobeat") ~ "hiphop",
                           str_detect(genre, "trance") ~ "edm",
                           genre == "new romantic" ~ "rock",
                           str_detect(genre, "chanson") ~ "folk",
                           genre == "neomelodici" ~ "folk",
                           genre == "canzone napoletana" ~ "folk",
                           genre == "dark clubbing" ~ "edm",
                           str_detect(genre, "tekk") ~ "edm",
                           genre == "vapor twitch" ~ "edm",
                           genre == "aussietronica" ~ "edm",
                           genre == "french shoegaze" ~ "rock",
                           genre == "italian alternative" ~ "rock",
                           genre == "tekno" ~ "edm",
                           genre == "acidcore" ~ "edm",
                           genre == "sped up" ~ "others",
                           genre == "neoperreo" ~ "hiphop",
                           str_detect(genre, "singer-songwriter") ~ "cantautor",
                           genre == "axe" ~ "folk",
                           genre == "bases de freestyle" ~ "rap",
                           str_detect(genre, "blues") ~ "rb",
                           genre == "copla" ~ "folk",
                           genre == "beatlesque" ~ "rock",
                           genre == "bossbeat" ~ "edm",
                           genre == "brooklyn drill" ~ "hiphop",
                           genre == "bubblegrunge" ~ "rock",
                           genre == "complextro" ~ "edm",
                           genre == "crank wave" ~ "edm",
                           str_detect(genre, "hardcore") ~ "rock",
                           genre == "epicore" ~ "rock",
                           genre == "deep latin christian" ~ "folk",
                           genre == "escape room" ~ "edm",
                           genre == "high vibe" ~ "others",
                           genre == "euskal musica" ~ "folk",
                           genre == "musica catalana" ~ "folk",
                           genre == "nordnorsk musikk" ~ "folk",
                           genre == "hands up" ~ "edm",
                           genre == "hi-nrg" ~ "edm",
                           genre == "nightcore" ~ "edm",
                           genre == "kavkaz" ~ "folk",
                           genre == "oriental classical" ~ "folk",
                           genre == "musica chihuahuense" ~ "folk",
                           genre == "musica portuguesa contemporanea" ~ "folk",
                           genre == "nova mpb" ~ "folk",
                           genre == "orchestra" ~ "others",
                           genre == "melbourne bounce international" ~ "edm",
                           genre == "russian rave" ~ "edm",
                           genre == "mod revival" ~ "rock",
                           genre == "new french touch" ~ "edm",
                           genre == "psychokore" ~ "edm",
                           genre == "quiet storm" ~ "rb",
                           genre == "sad lo-fi" ~ "indie",
                           genre == "variete francaise" ~ "pop",
                           genre == "wrestling" ~ "others",
                           genre == "desconocido" ~ "others",
                           .default = genre))

ggplot(spotify_raw, aes(artists_popularity, artists_followers))+
  geom_point(color = "#FF8D63") +
  scale_y_log10() +
  theme_minimal()
# 
# ggplot(spotify, aes(y=fade_in))+
#   geom_boxplot(fill = "#47CBFF") +
#   theme_minimal()
# 
# library(corrr)
# cor_cont <- spotify[var_cont] |> cor() |> round(2)
# library(corrplot)
# corplot <- cor_cont |>
#   corrplot(method = "number", tl.cex = 0.55, number.cex = 0.7, type = "lower")
# 
ggplot(spotify_raw, aes(as.factor(mode)))+
  geom_bar(aes(y = 100 * (..count..)/sum(..count..)), fill = "#FF8D63") +
  labs(y="Porcentaje",
       x="Major") +
  theme_minimal()
# 
var_cont <- c("acousticness", "artists_followers", "artists_popularity",
              "danceability", "duration_min", "energy", "fade_in",
              "fade_out", "liveness", "loudness", "num_sections",
              "popularity", "speechiness", "tempo", "valence")


TFM_palette3 = c("#FF8D63", "#47CBFF", "#E4A7F4")
vgraf_num<-list()
i=1
for (vari in var_cont){
  a<-ggplot(spotify, aes_string(vari)) +
    geom_histogram(fill = TFM_palette3[(i-1)%%3 + 1], alpha=1, bins=30) +
    theme_minimal()
  vgraf_num[[i]]<-a
  i=i+1
}
png(file="./figuras/variables numericas.png",res=80,width=800,height=600)
grid.arrange(grobs=vgraf_num,ncol=3,nrow=5)
dev.off()

var_cont <- c("acousticness", "artists_popularity",
              "danceability", "duration_min", "energy", "fade_in",
              "fade_out", "liveness", "loudness", "num_sections",
              "popularity", "speechiness", "tempo", "valence", "year")
var_cat <- c("genre", "collab", "explicit", "key", "major")

vgraf_num_liked<-list()
i=1
for (vari in var_cont){
  a<-ggplot(spotify, aes_string(vari, fill = "liked", color = "liked")) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values=TFM_palette3) +
    scale_color_manual(values=TFM_palette3) +
    theme_minimal() +
    if(i%%3!=0){guides(fill="none", color = "none")}
  # print(a)
  vgraf_num_liked[[i]]<-a
  i=i+1
}
png(file="./figuras/variables numericas_liked.png",res=80,width=800,height=1200)
grid.arrange(grobs=vgraf_num_liked,ncol=3,nrow=5, widths = c(1, 1, 1.25))
dev.off()

vgraf_cat_liked<-list()
vgraf_cat_liked[[1]] <- ggplot(spotify, aes(genre, fill = liked)) +
  geom_bar(position = "fill", width=1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "Porcentaje") +
  guides(fill = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
vgraf_cat_liked[[2]] <- ggplot(spotify, aes(as.factor(collab), fill = liked)) +
  geom_bar(position = "fill", width=1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "",
       x= "collab") +
  guides(fill = "none") +
  theme_minimal()
vgraf_cat_liked[[3]] <- ggplot(spotify, aes(as.factor(explicit), fill = liked)) +
  geom_bar(position = "fill", width=1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "",
       x = "explicit") +
  theme_minimal()
vgraf_cat_liked[[4]] <- ggplot(spotify, aes(as.factor(key), fill = liked)) +
  geom_bar(position = "fill", width=1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "Porcentaje",
       x = "key") +
  guides(fill = "none") +
  theme_minimal()
vgraf_cat_liked[[5]] <- ggplot(spotify, aes(as.factor(major), fill = liked)) +
  geom_bar(position = "fill", width=1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "",
       x = "major") +
  guides(fill = "none") +
  theme_minimal()

png(file="./figuras/variables categoricas_liked.png",res=80,width=800,height=600)
grid.arrange(grobs=vgraf_cat_liked,ncol=3,nrow=2, widths = c(1.5, 1, 1.25))
dev.off()

spotify_tree <- spotify |> select(-c("id", "role"))

# Árbol inicial
library(rpart)
library(rpart.plot)

arbol <- rpart(factor(liked) ~ ., data = spotify_tree,
               minbucket =50,method = "class",parms=list(split="gini"),cp=0)

rpart.plot(arbol,extra=105,nn=TRUE, box.palette = c("#FF8D63", "#47CBFF"))

# FEATURE ENGINEERING

spotifyfe <- spotify

# target encoding ponderado
source("funcion target_encod_binaria.R")
spotifyfe<-target_encod_bin(spotifyfe,"genre","liked",60,1,"tpondgenre")

# Rank Target Encoding

# Mejor para controlar sobreajuste, usa la salida del target encoder.

spotifyfe$rank<-rank(spotifyfe$tpondgenre)

# Arbol

library(rpart)
library(rpart.plot)
arbol1 <- rpart(factor(liked)~genre, data = spotifyfe,
                minbucket = 10, maxsurrogate=0,cp=0)

# Realizar la predicción del nodo en cada observación
prediarbol <- predict(arbol1, newdata = spotifyfe)

# Crear un dataframe
nuevo <- data.frame(prediarbol = prediarbol)
names(nuevo) <- c("predNo.arbol", "predYes.arbol")

# Unir el dataframecon el dataframe original
spotifyfe <- cbind(spotifyfe, nuevo$prediarbol.Yes)


library(h2o)

h2o.init()

dati<-spotifyfe[,c("genre", "liked")]

dati$genre<-as.factor(dati$genre)

train<-as.h2o(dati)

ae1 <- h2o.deeplearning(
  x = 1,training_frame = train,
  autoencoder = TRUE,
  hidden = 3,
  activation = 'Tanh'
)

ae1_codings <- h2o.deepfeatures(ae1, train, layer = 1)
ae1_codings

koko<-as.data.frame(ae1_codings)

names(koko) <- c("autoencoders.1", "autoencoders.2", "autoencoders.3")
spotifyfe<-cbind(spotifyfe,koko)

h2o.shutdown()

# Dummy

spotify_df<-data.frame(spotifyfe)
spotify_dum<-dummy.data.frame(spotify_df, "genre", sep = ".")

frecu<-plyr::ldply(spotify[,"genre"],function(x) t(rbind(names(table(x)),table(x))))
names(frecu)<-c("variable","nivel","frecuencia")
frecu$frecuencia<-as.numeric(frecu$frecuencia)
frecu20<-frecu[frecu$frecuencia<20,]
frecu20$dum<-paste(frecu20$variable,frecu20$nivel,sep=".")
listamal<-dput(frecu20$dum)
spotify_dum[,listamal]<-NULL

# Variables feature engineering

ggplot(spotifyfe, aes(tpondgenre, color=liked, fill = liked)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values=TFM_palette3) +
  scale_color_manual(values=TFM_palette3) +
  labs(y = "",
       x= "target encoding") +
  guides(color = "none") +
  theme_minimal()
ggplot(spotifyfe, aes(`nuevo$prediarbol.Yes`, color=liked, fill = liked)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values=TFM_palette3) +
  labs(y = "",
       x = "Predicción árbol") +
  guides(color = "none") +
  theme_minimal()
vautoencoders<-list()
vautoencoders[[1]] <- ggplot(spotifyfe, aes(autoencoders.1, autoencoders.2, color = liked)) +
  geom_point() +
  scale_color_manual(values=TFM_palette3) +
  labs(y = "autoencoders2",
       x = "autoencoders1") +
  guides(color = "none") +
  theme_minimal()
vautoencoders[[2]] <- ggplot(spotifyfe, aes(autoencoders.1, autoencoders.3, color = liked)) +
  geom_point() +
  scale_color_manual(values=TFM_palette3) +
  labs(y = "autoencoders3",
       x = "autoencoders1") +
  guides(color = "none") +
  theme_minimal()
vautoencoders[[3]] <- ggplot(spotifyfe, aes(autoencoders.2, autoencoders.3, color = liked)) +
  geom_point() +
  scale_color_manual(values=TFM_palette3) +
  labs(y = "autoencoders3",
       x = "autoencoders2") +
  theme_minimal()

png(file="./figuras/feature_engineering.png",res=100,width=600,height=200)
grid.arrange(grobs=vautoencoders,ncol=3,nrow=1, widths = c(1, 1, 1.25))
dev.off()

# Escalar
escalar_01 <- function(x, min, max) { return((x - min) / (max - min))}

spotify_scaled <- spotify_dum
spotify_scaled$artists_followers <- escalar_01(spotify_dum$artists_followers, min=0, max = 112710440)
spotify_scaled$artists_popularity <- escalar_01(spotify_dum$artists_popularity, min=0, max = 100)
spotify_scaled$autoencoders.1 <- escalar_01(spotify_dum$autoencoders.1, min=min(spotify_dum$autoencoders.1),
                                            max = max(spotify_dum$autoencoders.1))
spotify_scaled$autoencoders.2 <- escalar_01(spotify_dum$autoencoders.2, min=min(spotify_dum$autoencoders.2),
                                            max = max(spotify_dum$autoencoders.2))
spotify_scaled$autoencoders.3 <- escalar_01(spotify_dum$autoencoders.3, min=min(spotify_dum$autoencoders.3),
                                            max = max(spotify_dum$autoencoders.3))
spotify_scaled$popularity <- escalar_01(spotify_dum$popularity, min=0, max = 100)
spotify_scaled$loudness <- escalar_01(spotify_dum$loudness, min=-61, max = 1)
spotify_scaled$year <- escalar_01(spotify_dum$year, min=min(spotify_dum$year),
                                  max = 2023)
spotify_scaled$num_sections <- escalar_01(spotify_dum$num_sections, min=1, max = 40)
spotify_scaled$key <- escalar_01(spotify_dum$key, min=0, max = 11)
spotify_scaled$tempo <- escalar_01(spotify_dum$tempo, min=0, max = 250)
spotify_scaled$duration_min <- escalar_01(spotify_dum$duration_min, min=0, max = 15)
spotify_scaled$fade_in <- escalar_01(spotify_dum$fade_in, min=0,
                                  max = max(spotify_dum$fade_in))
spotify_scaled$fade_out <- escalar_01(spotify_dum$fade_out, min=0,
                                     max = max(spotify_dum$fade_out))

archivo1 <- spotify_scaled
vardep <- c("liked")
varindep <- c("artists_followers", "artists_popularity", "num_sections", 
              "danceability", "energy", "key", "loudness", "major", "speechiness", 
              "acousticness", "liveness", "valence", "tempo", "fade_in", "fade_out", 
              "explicit", "popularity", "genre.cantautor", "genre.edm", "genre.folk", 
              "genre.hiphop", "genre.indie", "genre.others", "genre.pop", "genre.rap", 
              "genre.rb", "genre.rock", "year", "duration_min", "collab", "tpondgenre",
              "rank", "nuevo.prediarbol.Yes", "autoencoders.1", "autoencoders.2", "autoencoders.3")
varothers <- c("id", "role")
x <- archivo1[,varindep]
y <- archivo1[,vardep]
library(foreign)
write.dbf(archivo1,"C:/Users/charl/Documents/CloudStation/Uni/Master/TFM/spotify_scaled.dbf")

# Selección de variables
data_selec <- archivo1[,c(varindep,vardep)]

full<-glm(factor(liked)~.,data=data_selec,family = binomial(link="logit"))
null<-glm(factor(liked)~1,data=data_selec,family = binomial(link="logit"))

library(MASS)

selecAIC<-stepAIC(null,scope=list(upper=full),direction="both",trace=FALSE)

dput(names(selecAIC[[1]]))

# c("tpondgenre", "speechiness", "popularity", "explicit", "duration_min",
#   "valence", "acousticness", "danceability", "artists_popularity",
#   "genre.others", "artists_followers")

# BIC
selecBIC<-stepAIC(null,scope=list(upper=full),direction="both",family = binomial(link="logit"),trace=FALSE, k=8)
dput(names(selecBIC[[1]]))

# c("tpondgenre", "speechiness", "popularity", "explicit",
#   "duration_min", "valence")

# BORUTA

archivo2 <- data_selec
archivo2$liked<-as.factor(archivo2$liked)
# También vale como Filter
library(Boruta)
out.boruta <- Boruta(liked~., data = archivo2)

sal_boruta<-data.frame(out.boruta$finalDecision)

sal_boruta2<-sal_boruta[which(sal_boruta$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]

length(dput(row.names(sal_boruta2)))

# c("artists_followers", "artists_popularity", "num_sections",
#   "danceability", "energy", "speechiness", "acousticness", "valence",
#   "fade_out", "explicit", "popularity", "genre.hiphop", "genre.pop",
#   "year", "duration_min", "tpondgenre", "rank", "nuevo.prediarbol.Yes",
#   "autoencoders.1", "autoencoders.2", "autoencoders.3")

# MXM

library(MXM)

SES1 <- SES(vardep, archivo2, max_k = 3, hash = TRUE,
            test = "testIndLogistic")

dput(names(archivo2[,c(SES1@selectedVars)]))

# c("energy", "speechiness", "explicit", "popularity", "duration_min",
#   "tpondgenre")

source("funcion steprepetido binaria.R")

lista<-steprepetidobinaria(data=data_selec,vardep=c("liked"),
                           listconti=varindep,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="AIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# c("tpondgenre", "speechiness", "popularity", "duration_min",
#   "danceability", "explicit", "liveness", "acousticness", "valence",
#   "artists_followers", "artists_popularity", "genre.others")


lista<-steprepetidobinaria(data=data_selec,vardep=c("liked"),
                           listconti=varindep,
                           sinicio=12345,sfinal=12385,porcen=0.8,criterio="BIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]])

# c("tpondgenre", "speechiness", "popularity", "explicit", "duration_min",
#   "valence")

# AQUÍ USO DETACH PUES HAY PAQUETES CON FUNCIONES CON LOS MISMOS nombres1Y DAN PROBLEMAS

detach(package:MXM)
detach(package:Boruta)
library(caret)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(x, as.factor(y), sizes=c(1:20), rfeControl=control)
cosa<-as.data.frame(results$results)
results

# Resultados en gráfico
ggplot(cosa,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+
  scale_y_continuous(breaks = round(cosa$Accuracy, digits = 4)) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")


# Pongo a partir de 4 variables
cosa2<-cosa[c(5:20),]
ggplot(cosa2,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+
  scale_y_continuous(breaks = round(cosa$Accuracy, digits = 4)) +
  scale_x_continuous(breaks = cosa$Variables)+labs(title="RFE")


selecrfe<-results$optVariables[1:10]

dput(selecrfe)


# c("speechiness", "popularity", "tpondgenre", "rank", "nuevo.prediarbol.Yes",
#   "artists_popularity", "year", "autoencoders.3", "artists_followers",
#   "explicit")

library(randomForest)
# PRUEBA CON BAGGING
rfgrid<-expand.grid(mtry=c(36))
set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all")
rf<- train(factor(liked)~.,data=data_selec,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf
# CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseGini),]
tabla

lista<-dput(rownames(tabla))

set.seed(1234)
vacio2<-data.frame()

for (i in (5:25)){
  varis<-lista[1:i]
  data2<-data_selec[,c(varis,vardep)]
  rfgrid<-expand.grid(mtry=c(i))

  control<-trainControl(method = "cv",number=10,savePredictions = "all")

  rf<- train(factor(liked)~.,data=data2,
             method="rf",trControl=control,tuneGrid=rfgrid,
             linout = F,ntree=500,nodesize=10,replace=TRUE,
             importance=TRUE)

  a<-rf$results$Accuracy
  vacio <- data.frame(Variables = i, Accuracy= a)
  vacio2<-rbind(vacio,vacio2)}
vacio2 <- arrange(vacio2, Variables)

ggplot(vacio2,aes(y=Accuracy, x=Variables))+geom_point()+geom_line()+
  scale_x_continuous(breaks = vacio2$Variables)+
  scale_y_continuous(breaks = round(vacio2$Accuracy, digits = 4)) +  labs(title="BAGGING")

vacio2

selecrandomforest<-lista[1:16]

dput(selecrandomforest)

# c("speechiness", "popularity", "tpondgenre", "duration_min",
#   "rank", "danceability", "artists_followers", "energy", "artists_popularity",
#   "valence", "year", "acousticness", "fade_out")
#
# c("speechiness", "popularity", "tpondgenre", "duration_min",
#   "rank", "danceability", "artists_followers", "energy", "artists_popularity",
#   "valence", "year", "acousticness", "fade_out", "tempo", "liveness",
#   "loudness")

#Comparación

source("cruzadas avnnet y log binaria.R")
data<-data_selec

medias1<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("tpondgenre", "speechiness", "popularity","explicit",
                                      "duration_min", "valence", "acousticness","danceability",
                                      "artists_popularity", "genre.others", "artists_followers"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias1$modelo="StepAIC"

medias2<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("tpondgenre", "speechiness", "popularity", "explicit",
                                      "duration_min", "valence"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias2$modelo="StepBIC"

medias3<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("artists_followers", "artists_popularity", "num_sections",
                                      "danceability", "energy", "speechiness", "acousticness",
                                      "valence", "fade_out", "explicit", "popularity",
                                      "genre.hiphop", "genre.pop", "year", "duration_min",
                                      "tpondgenre", "rank", "nuevo.prediarbol.Yes",
                                      "autoencoders.1", "autoencoders.2", "autoencoders.3"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias3$modelo="Boruta"

medias4<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("energy", "speechiness", "explicit", "popularity",
                                      "duration_min", "tpondgenre"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias4$modelo="SES"

medias5<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("tpondgenre", "speechiness", "popularity", "duration_min",
                                      "danceability", "explicit", "liveness", "acousticness", "valence",
                                      "artists_followers", "artists_popularity", "genre.others"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias5$modelo="StepRepAIC"

medias6<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("tpondgenre", "speechiness", "popularity", "explicit", "duration_min",
                                      "valence"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias6$modelo="StepRepBIC"

medias7<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("speechiness", "popularity", "tpondgenre", "rank", "nuevo.prediarbol.Yes",
                                      "artists_popularity", "year", "autoencoders.3", "artists_followers",
                                      "explicit"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias7$modelo="RFE"

medias8<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("speechiness", "popularity", "tpondgenre", "duration_min",
                                      "rank", "danceability", "artists_followers", "energy", "artists_popularity",
                                      "valence", "year", "acousticness", "fade_out"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias8$modelo="Bag13"

medias9<-cruzadalogistica(data=data, vardep="liked",
                          listconti=c("speechiness", "popularity", "tpondgenre", "duration_min",
                                      "rank", "danceability", "artists_followers", "energy", "artists_popularity",
                                      "valence", "year", "acousticness", "fade_out", "tempo", "liveness",
                                      "loudness"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias9$modelo="Bag16"

medias10<-cruzadalogistica(data=data, vardep="liked",
                           listconti=c("artists_followers", "artists_popularity", "num_sections",
                                       "danceability", "energy", "key", "loudness", "major", "speechiness",
                                       "acousticness", "liveness", "valence", "tempo", "fade_in", "fade_out",
                                       "explicit", "popularity", "genre.cantautor", "genre.edm", "genre.folk",
                                       "genre.hiphop", "genre.indie", "genre.others", "genre.pop", "genre.rap",
                                       "genre.rb", "genre.rock", "year", "duration_min", "collab", "tpondgenre",
                                       "rank", "nuevo.prediarbol.Yes", "autoencoders.1", "autoencoders.2",
                                       "autoencoders.3"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias10$modelo="todas"

medias11<-cruzadalogistica(data=data, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity", "explicit", "energy",
                                       "danceability", "artists_popularity"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias11$modelo="SAS"

medias12<-cruzadalogistica(data=data, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity", "duration_min",
                                       "explicit", "valence", "artists_popularity"),
                           listclass=c(""),grupos=4,sinicio=1234,repe=10)

medias12$modelo="masrep"

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9,medias10,medias11,medias12)

nvar = c(13,16,21,7,10,7,6,11,6,12,6,36)
nvar2 <- paste(nvar, "v.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_auc <- max(union1$auc)
min_auc <- min(union1$auc)
num_modelos <- length(unique(union1$modelo))

# Crear el boxplot y agregar las etiquetas
par(cex.axis=0.6,las=2)
boxplot(data=union1,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_auc, max_auc*1.001))
text(x = seq(1:num_modelos), y = rep(max_auc, num_modelos), labels = nvar2, pos = 3,col="red")
axis(2, at=pretty(range(union1$auc),n=20))

union1 |> group_by(modelo) |>
  summarise(mean = mean(1-tasa)) |>
  ungroup()

# c("tpondgenre", "speechiness", "popularity",
#   "explicit","duration_min", "valence")

## Partición Train-Test

training <- archivo1 |> 
  filter(role=="train") |> 
  dplyr::select(c("tpondgenre", "speechiness", "popularity",
                  "explicit","duration_min", "valence", "liked"))

write.dbf(training,"C:/Users/charl/Documents/CloudStation/Uni/Master/TFM/training.dbf")

set.seed(12345)
# Validación cruzada
control<-trainControl(method = "repeatedcv",number=4,savePredictions = "all",classProbs=TRUE)

logi<- train(factor(liked) ~ tpondgenre + speechiness + popularity +
                  explicit + duration_min + valence,
             data=training,method="glm",trControl=control)
logi
sal<-logi$pred

# MEDIDAS CON PUNTO DE CORTE 0.5

confusionMatrix(reference=sal$obs,data=sal$pred, positive="Yes")

# REDES NEURONALES

train <- training
control<-trainControl(method = "cv",
                      number=4,savePredictions = "all")

set.seed(123)
nnetgrid <-expand.grid(size=c(3,5,8,10),
                       decay=c(0.1,0.01,0.001),bag=FALSE)

completo<-data.frame()
listaiter<-c(5,10,15,20,25,30,40,50,75,100,250,500,1000,2500,5000)

for (iter in listaiter){
  rednnet<- train(liked~.,
                  data=train,
                  method="avNNet",linout = FALSE,maxit=iter,
                  trControl=control,repeats=5,tuneGrid=nnetgrid,trace=F)
  rednnet$results$itera<-iter
  completo<-rbind(completo,rednnet$results)
}

completo<-completo[order(completo$Accuracy),]

ggplot(completo, aes(x=factor(itera), y=Accuracy,
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette3) +
  labs(x = "iteraciones",
       color = "decay",
       pch = "size")

ggplot(completo |> filter(itera==20), aes(x=factor(size), y=Accuracy,
                     color=factor(decay))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette3) +
  labs(x = "nodos",
       color = "decay")


source("cruzadas avnnet y log binaria.R")
set.seed(123)
medias15<-cruzadaavnnetbin(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10,
                          size=c(3),decay=c(0.01),repeticiones=5,itera=20)

medias15$modelo="3nodos"

medias16<-cruzadaavnnetbin(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                          listclass=c(""),grupos=4,sinicio=1234,repe=10,
                          size=c(5),decay=c(0.01),repeticiones=5,itera=20)

medias16$modelo="5nodos"

medias17<-cruzadaavnnetbin(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),grupos=4,sinicio=1234,repe=10,
                           size=c(8),decay=c(0.01),repeticiones=5,itera=20)

medias17$modelo="8nodos"

medias18<-cruzadaavnnetbin(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),grupos=4,sinicio=1234,repe=10,
                           size=c(10),decay=c(0.01),repeticiones=5,itera=20)

medias18$modelo="10nodos"

union3<-rbind(medias15,medias16,medias17,medias18)

par(cex.axis=1, las = 1)
boxplot(data=union3,tasa~modelo,col="#FF8D63",main="TASA DE FALLOS REDES")
boxplot(data=union3,auc~modelo,col="#FF8D63",main="AUC")

union3 |> group_by(modelo) |>
  summarise(mean = mean(1-tasa)) |>
  ungroup()

# Bagging

library(randomForest)
rfgrid<-expand.grid(mtry=c(2,3,4,5,6))

set.seed(123)

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)

completo<-data.frame()
listntree<-c(5,10,15,20,30,50,75,100,250,500,1000,3000)

for (ntree in listaiter){
  rf<- train(data=train,
             factor(liked)~.,
             method="rf",trControl=control,tuneGrid=rfgrid,
             linout = FALSE,ntree=ntree,nodesize=30,replace=TRUE, importance=T)
  rf$results$itera<-ntree
  completo<-rbind(completo,rf$results)
}

completo<-completo[order(completo$Accuracy),]

TFM_palette5 = c("#FF8D63", "#47CBFF", "#E4A7F4", "#FFD700", "#D2B48C")
ggplot(completo, aes(x=factor(itera), y=Accuracy,
                     color=factor(mtry))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette5) +
  labs(x = "ntree",
       color = "mtry")


source("cruzada rf binaria.R")

set.seed(123)
medias19<-cruzadarfbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=10,
                      mtry=3,ntree=75,replace=TRUE,sampsize=100)

medias19$modelo="rf100"


medias20<-cruzadarfbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=10,
                      mtry=3,ntree=75,replace=TRUE,sampsize=500)

medias20$modelo="rf500"

medias21<-cruzadarfbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                      listclass=c(""),
                      grupos=4,sinicio=1234,repe=20,nodesize=10,
                      mtry=3,ntree=75,replace=TRUE,sampsize=1000)


medias21$modelo="rf1000"

medias22<-cruzadarfbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                       listclass=c(""),
                       grupos=4,sinicio=1234,repe=20,nodesize=10,
                       mtry=3,ntree=75,replace=TRUE)

medias22$modelo="rfBASE"

union4<-rbind(medias19,medias20,medias21,medias22)

par(cex.axis=1, las =1)
boxplot(data=union4,tasa~modelo,main="TASA FALLOS",col="#FF8D63")
boxplot(data=union4,auc~modelo,main="AUC",col="#FF8D63")

# Gradient Boosting

set.seed(123)

gbmgrid<-expand.grid(shrinkage=c(0.2,0.1,0.01,0.001),
                     n.minobsinnode=c(5,10,25,50,75),
                     n.trees=c(10,50,75,100,500,1000,5000),
                     interaction.depth=c(2))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)

gbm<- train(factor(liked)~.,data=train,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

results <- gbm$results
ggplot(results, aes(x=factor(n.trees), y=Accuracy,
                    color=factor(shrinkage),
                    pch=factor(n.minobsinnode))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette5) +
  labs(x = "n.trees",
       color = "shrinkage",
       pch = "n.minobsinnode")

results |> group_by(as.factor(n.minobsinnode)) |>
  summarise(mean = mean(Accuracy)) |>
  ungroup()

source ("cruzada gbm binaria.R")

medias29<-cruzadagbmbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                       listclass=c(""),
                       grupos=4,sinicio=123,repe=10,
                       n.minobsinnode=50,shrinkage=0.2,n.trees=75,interaction.depth=2)

medias29$modelo="gbm1"

medias30<-cruzadagbmbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=123,repe=10,
                        n.minobsinnode=50,shrinkage=0.1,n.trees=100,interaction.depth=2)

medias30$modelo="gbm2"

medias31<-cruzadagbmbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=123,repe=10,
                        n.minobsinnode=50,shrinkage=0.01,n.trees=1000,interaction.depth=2)

medias31$modelo="gbm3"

medias32<-cruzadagbmbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=123,repe=10,
                        n.minobsinnode=50,shrinkage=0.001,n.trees=5000,interaction.depth=2)

medias32$modelo="gbm4"

union6<-rbind(medias29,medias30,medias31,medias32)

accu = c(0.6781,0.6794,0.6792,0.6786)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union6$auc)
min_tasa <- min(union6$auc)
num_modelos <- length(unique(union6$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union6,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.005))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union6$auc),n=20))

# Xgboost

set.seed(123)

xgbmgrid<-expand.grid(
  min_child_weight=50, eta=0.01, nrounds=1000,
  max_depth=c(3,5,7,10),
  gamma=c(0,2,5,10),
  colsample_bytree=c(0.75,0.9,1),
  subsample=c(0.75,0.9,1))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE)

xgbm<- train(factor(liked)~.,data=train,
             method="xgbTree",trControl=control,
             tuneGrid=xgbmgrid,verbose=FALSE)


results <- xgbm$results |> filter(colsample_bytree==1 & subsample==1)
ggplot(results, aes(x=factor(max_depth), y=Accuracy,
                    color=factor(gamma))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette5) +
  labs(x = "max_depth",
       color = "gamma") +
  theme(legend.position = "top")

source ("cruzada xgboost binaria.R")

medias37<-cruzadaxgbmbin(data=train, vardep="liked",
                         listconti=c("tpondgenre", "speechiness", "popularity",
                                     "explicit","duration_min", "valence"),
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=10,
                         min_child_weight=50,eta=0.01,nrounds=1000,max_depth=3,
                         gamma=2,colsample_bytree=1,subsample=1)

medias37$modelo="xgbm1"

medias38<-cruzadaxgbmbin(data=train, vardep="liked",
                         listconti=c("tpondgenre", "speechiness", "popularity",
                                     "explicit","duration_min", "valence"),
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=10,
                         min_child_weight=50,eta=0.01,nrounds=1000,max_depth=5,
                         gamma=5,colsample_bytree=1,subsample=1)

medias38$modelo="xgbm2"


medias39<-cruzadaxgbmbin(data=train, vardep="liked",
                         listconti=c("tpondgenre", "speechiness", "popularity",
                                     "explicit","duration_min", "valence"),
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=10,
                         min_child_weight=50,eta=0.01,nrounds=1000,max_depth=7,
                         gamma=5,colsample_bytree=1,subsample=1)

medias39$modelo="xgbm3"

medias40<-cruzadaxgbmbin(data=train, vardep="liked",
                         listconti=c("tpondgenre", "speechiness", "popularity",
                                     "explicit","duration_min", "valence"),
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=10,
                         min_child_weight=50,eta=0.01,nrounds=1000,max_depth=10,
                         gamma=5,colsample_bytree=1,subsample=1)

medias40$modelo="xgbm4"

medias41<-cruzadaxgbmbin(data=train, vardep="liked",
                         listconti=c("tpondgenre", "speechiness", "popularity",
                                     "explicit","duration_min", "valence"),
                         listclass=c(""),
                         grupos=4,sinicio=1234,repe=10,
                         min_child_weight=50,eta=0.01,nrounds=1000,max_depth=10,
                         gamma=2,colsample_bytree=0.9,subsample=0.75)

medias41$modelo="xgbm5"

union8<-rbind(medias37,medias38,medias39,medias40,medias41)

accu = c(0.6771,0.6756,0.6756,0.6756,0.6740)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union8$auc)
min_tasa <- min(union8$auc)
num_modelos <- length(unique(union8$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union8,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.001))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union8$auc),n=20))

#SVM

#SVM lineal

#  SVM LINEAL: SOLO PARÁMETRO C

set.seed(123)
SVMgrid<-expand.grid(C=c(0.005,0.007,0.01,0.05,0.1,1,5,10))

control<-trainControl(method = "cv",number=4,savePredictions = "all")

SVM<- train(data=train,factor(liked)~.,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results |> ggplot(aes(C, Accuracy)) +
  geom_line(col="#FF8D63", lwd = 1) + geom_point(col="#FF8D63", size=1.5)

source ("cruzada SVM binaria lineal.R")

medias42<-cruzadaSVMbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=10,C=0.001)

medias42$modelo="C=0.001"

medias43<-cruzadaSVMbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=10,C=0.005)

medias43$modelo="C=0.005"

medias44<-cruzadaSVMbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=10,C=0.01)

medias44$modelo="C=0.01"

medias45<-cruzadaSVMbin(data=train, vardep="liked",
                        listconti=c("tpondgenre", "speechiness", "popularity",
                                    "explicit","duration_min", "valence"),
                        listclass=c(""),
                        grupos=4,sinicio=1234,repe=10,C=0.02)

medias45$modelo="C=0.02"

union9<-rbind(medias42,medias43,medias44,medias45)

accu = c(0.6642,0.6809,0.6800,0.6781)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union9$auc)
min_tasa <- min(union9$auc)
num_modelos <- length(unique(union9$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union9,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.001))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union9$auc),n=20))

#  SVM Polinomial: PARÁMETROS C, degree, scale

set.seed(123)
SVMgrid<-expand.grid(C=c(0.005,0.01,0.02,0.1,1,5,10),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all")


SVM<- train(data=train,factor(liked)~.,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

results <- SVM$results
ggplot(results, aes(x=factor(C), y=Accuracy,
                    color=factor(scale),
                    pch=factor(degree))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette5) +
  labs(x = "C",
       color = "scale",
       pch="degree") +
  theme(legend.position = "top")

source ("cruzada SVM binaria polinomial.R")

medias47<-cruzadaSVMbinPoly(data=train, vardep="liked",
                            listconti=c("tpondgenre", "speechiness", "popularity",
                                        "explicit","duration_min", "valence"),
                            listclass=c(""),
                            grupos=4,sinicio=1234,repe=10,
                            C=0.005,degree=3,scale=0.1)

medias47$modelo="c=0.005"

medias48<-cruzadaSVMbinPoly(data=train, vardep="liked",
                            listconti=c("tpondgenre", "speechiness", "popularity",
                                        "explicit","duration_min", "valence"),
                            listclass=c(""),
                            grupos=4,sinicio=1234,repe=10,
                            C=0.01,degree=3,scale=0.1)

medias48$modelo="C=0.01"

medias49<-cruzadaSVMbinPoly(data=train, vardep="liked",
                            listconti=c("tpondgenre", "speechiness", "popularity",
                                        "explicit","duration_min", "valence"),
                            listclass=c(""),
                            grupos=4,sinicio=1234,repe=10,
                            C=0.02,degree=3,scale=0.1)

medias49$modelo="C=0.02"

union10<-rbind(medias47,medias48,medias49)

accu = c(0.6771,0.6810,0.6805)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union10$auc)
min_tasa <- min(union10$auc)
num_modelos <- length(unique(union10$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union10,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.001))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union10$auc),n=20))

#  SVM RBF: PARÁMETROS C, sigma

set.seed(123)
SVMgrid<-expand.grid(C=c(0.005,0.01,0.02,0.1,1,5,10),
                     sigma=c(0.001,0.005,0.01,0.05))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all")


SVM<- train(data=train,factor(liked)~.,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)
SVM
results <- SVM$results
ggplot(results, aes(x=factor(C), y=Accuracy,
                    color=factor(sigma))) +
  geom_point(position=position_dodge(width=0.5),size=3) +
  scale_color_manual(values=TFM_palette5) +
  labs(x = "C",
       color = "sigma") +
  theme(legend.position = "top")

source ("cruzada SVM binaria RBF.R")

medias50<-cruzadaSVMbinRBF(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=10, C=5,sigma=0.001)

medias50$modelo="rbf1"

medias51<-cruzadaSVMbinRBF(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=10, C=1,sigma=0.005)

medias51$modelo="rbf2"

medias52<-cruzadaSVMbinRBF(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=10, C=0.1,sigma=0.01)

medias52$modelo="rbf3"

medias53<-cruzadaSVMbinRBF(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=4,sinicio=1234,repe=10, C=0.1,sigma=0.02)

medias53$modelo="rbf4"

union11<-rbind(medias50,medias51,medias52,medias53)

accu = c(0.6804,0.6822,0.67412,0.6799)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union11$auc)
min_tasa <- min(union11$auc)
num_modelos <- length(unique(union11$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union11,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.001))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union11$auc),n=20))


source ("cruzada SVM binaria lineal.R")
source ("cruzada SVM binaria polinomial.R")
source ("cruzada SVM binaria RBF.R")

medias54<-cruzadaSVMbin(data=train, vardep="liked",
                       listconti=c("tpondgenre", "speechiness", "popularity",
                                   "explicit","duration_min", "valence"),
                       listclass=c(""),
                       grupos=10,sinicio=1234,repe=10,C=0.005)

medias54$modelo="Lineal"


medias55<-cruzadaSVMbinPoly(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=10,sinicio=1234,repe=10,
                           C=0.01,degree=3,scale=0.1)

medias55$modelo="polinomial"


medias56<-cruzadaSVMbinRBF(data=train, vardep="liked",
                           listconti=c("tpondgenre", "speechiness", "popularity",
                                       "explicit","duration_min", "valence"),
                           listclass=c(""),
                           grupos=10,sinicio=1234,repe=10,
                           C=1,sigma=0.005)

medias56$modelo="RBF"

union12<-rbind(medias54,medias55,medias56)

accu = c(0.6844,0.6851,0.6839)
accu2 <- paste(accu, "accu.")
# ESTO PARA CONTROLAR LOS EJES Y POSICION DE LA ETIQUETA
max_tasa <- max(union12$auc)
min_tasa <- min(union12$auc)
num_modelos <- length(unique(union11$modelo))
# Crear el boxplot y agregar las etiquetas
par(cex.axis=1,las=1)
boxplot(data=union12,col="#FF8D63",auc~modelo,main="AUC",ylim=c(min_tasa, max_tasa*1.0005))
text(x = seq(1:num_modelos), y = rep(max_tasa, num_modelos), labels = accu2, pos = 3)
axis(2, at=pretty(range(union11$auc),n=20))

# Ensamblado

source("cruzadas ensamblado binaria fuente.R")

vardep<-"liked"
listconti<-c("tpondgenre", "speechiness", "popularity",
             "explicit","duration_min", "valence")
listclass<-c("")
grupos<-10
sinicio<-1234
repe<-10

# APLICACIÓN CRUZADAS PARA ENSAMBLAR

medias57<-cruzadalogistica(data=train,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe)

medias57bis<-as.data.frame(medias57[1])
medias57bis$modelo<-"Logistica"
predi1<-as.data.frame(medias57[2])
predi1$logi<-predi1$Yes

medias58<-cruzadaavnnetbin(data=train,
                          vardep=vardep,listconti=listconti,
                          listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                          size=c(5),decay=c(0.01),repeticiones=10,itera=20)

medias58bis<-as.data.frame(medias58[1])
medias58bis$modelo<-"red"
predi2<-as.data.frame(medias58[2])
predi2$avnnet<-predi2$Yes


medias59<-cruzadarfbin(data=train,
                      vardep=vardep,listconti=listconti,
                      listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                      mtry=3,ntree=75,nodesize=50,replace=TRUE,sampsize = 100)


medias59bis<-as.data.frame(medias59[1])
medias59bis$modelo<-"rf"
predi3<-as.data.frame(medias59[2])
predi3$rf<-predi3$Yes

medias60<-cruzadagbmbin(data=train,
                       vardep=vardep,listconti=listconti,
                       listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                       n.minobsinnode=50,shrinkage=0.01,n.trees=1000,interaction.depth=2)

medias60bis<-as.data.frame(medias60[1])
medias60bis$modelo<-"gbm"
predi4<-as.data.frame(medias60[2])
predi4$gbm<-predi4$Yes

medias61<-cruzadaxgbmbin(data=train,
                        vardep=vardep,listconti=listconti,
                        listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                        min_child_weight=50,eta=0.01,nrounds=1000,max_depth=3,
                        gamma=2,colsample_bytree=1,subsample=1,
                        alpha=0,lambda=0)


medias61bis<-as.data.frame(medias61[1])
medias61bis$modelo<-"xgbm"
predi5<-as.data.frame(medias61[2])
predi5$xgbm<-predi5$Yes


medias62<-cruzadaSVMbinPoly(data=train,
                            vardep=vardep,listconti=listconti,
                            listclass=listclass,grupos=grupos,sinicio=sinicio,repe=repe,
                            C=0.01,degree=3,scale=0.1)

medias62bis<-as.data.frame(medias62[1])
medias62bis$modelo<-"SVMpoli"
predi6<-as.data.frame(medias62[2])
predi6$svmPoly<-predi6$Yes

union13<-rbind(medias57bis,medias58bis,medias59bis,
               medias60bis,medias61bis,medias62bis)

par(cex.axis=1)

boxplot(data=union13,auc~modelo,col="#FF8D63",main='AUC')

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6)
# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]
unigraf<-unipredi[unipredi$Rep=="Rep01",]
# Correlaciones entre predicciones de cada algoritmo individual
solos<-c("logi", "avnnet","rf","gbm","xgbm", "svmPoly")
mat<-unigraf[,solos]
matrizcorr<-cor(mat)
matrizcorr
library(corrplot)
corrplot(matrizcorr, type = "upper", order = "hclust",
         col = colorRampPalette(c("white", "#FF8D63", "#47CBFF"))(100),
         tl.col = "black", tl.srt = 45,cl.lim=c(0.7,1),is.corr=FALSE)

# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS ARCHIVOS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...

# Construccion de ensamblados

unipredi$predi7<-(unipredi$logi+unipredi$avnnet)/2
unipredi$predi8<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi9<-(unipredi$logi+unipredi$rf)/2
unipredi$predi10<-(unipredi$logi+unipredi$xgbm)/2
unipredi$predi11<-(unipredi$logi+unipredi$svmPoly)/2

unipredi$predi12<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi13<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi14<-(unipredi$avnnet+unipredi$xgbm)/2
unipredi$predi15<-(unipredi$avnnet+unipredi$svmPoly)/2

unipredi$predi16<-(unipredi$gbm+unipredi$rf)/2
unipredi$predi17<-(unipredi$gbm+unipredi$xgbm)/2
unipredi$predi18<-(unipredi$gbm+unipredi$svmPoly)/2

unipredi$predi19<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$predi20<-(unipredi$xgbm+unipredi$svmPoly)/2

unipredi$predi21<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi22<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3
unipredi$predi23<-(unipredi$logi+unipredi$avnnet+unipredi$xgbm)/3
unipredi$predi24<-(unipredi$logi+unipredi$avnnet+unipredi$svmPoly)/3

unipredi$predi25<-(unipredi$logi+unipredi$gbm+unipredi$rf)/3
unipredi$predi26<-(unipredi$logi+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi27<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3

unipredi$predi28<-(unipredi$logi+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi29<-(unipredi$logi+unipredi$xgbm+unipredi$svmPoly)/3

unipredi$predi30<-(unipredi$avnnet+unipredi$gbm+unipredi$rf)/3
unipredi$predi31<-(unipredi$avnnet+unipredi$gbm+unipredi$xgbm)/3
unipredi$predi32<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3

unipredi$predi33<-(unipredi$avnnet+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi34<-(unipredi$avnnet+unipredi$xgbm+unipredi$svmPoly)/3

unipredi$predi35<-(unipredi$gbm+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi36<-(unipredi$gbm+unipredi$xgbm+unipredi$svmPoly)/3

unipredi$predi37<-(unipredi$logi+unipredi$gbm+unipredi$rf+unipredi$avnnet)/4
unipredi$predi38<-(unipredi$logi+unipredi$gbm+unipredi$xgbm+unipredi$avnnet)/4
unipredi$predi39<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly+unipredi$avnnet)/4


# dput(names(unipredi))

listado<-c("logi", "avnnet", "rf", "gbm", "xgbm", "svmPoly",
           "predi7", "predi8", "predi9", "predi10", "predi11",
           "predi12", "predi13", "predi14", "predi15", "predi16",
           "predi17", "predi18", "predi19", "predi20", "predi21",
           "predi22", "predi23", "predi24", "predi25", "predi26",
           "predi27", "predi28", "predi29", "predi30", "predi31",
           "predi32", "predi33", "predi34", "predi35", "predi36",
           "predi37", "predi38", "predi39")

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0
unipredi2<-unipredi
repeticiones<-nlevels(factor(unipredi2$Rep))
unipredi2$Rep<-as.factor(unipredi2$Rep)
unipredi2$Rep<-as.numeric(unipredi2$Rep)


medias0<-data.frame(c())
for (prediccion in listado)
{
  unipredi2$proba<-unipredi2[,prediccion]
  unipredi2[,prediccion]<-ifelse(unipredi2[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi2[(unipredi2$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-suppressMessages(auc(archi$obs,archi$proba))
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# Finalmente boxplot

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,tasa~modelo,col="#FF8D63", main='TASA FALLOS')

tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(tasa=mean(tasa))

tablamedias<-as.data.frame(tablamedias[order(tablamedias$tasa),])

# Para AUC se utiliza la variable auc del archivo medias0

medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,auc~modelo,col="#FF8D63", main='AUC')

tablamedias2<-medias0 %>%
  group_by(modelo) %>%
  summarize(auc=mean(auc))

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]


listadobis<-c("predi23", "predi32", "predi34", "predi18", "predi38", "predi39")

medias0$modelo<-as.character(medias0$modelo)

mediasver<-medias0[medias0$modelo %in% listadobis,]


mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))

par(cex.axis=0.9,las=2)
boxplot(data=mediasver,auc~modelo,col="#FF8D63",main='AUC')

listamodelos<-c("logi","avnnet", "rf", "gbm", "xgbm", "svmPoly", "predi18")

medias0$modelo<-as.character(medias0$modelo)

mediasver2<-medias0[medias0$modelo %in% listamodelos,]


mediasver2$modelo <- with(mediasver2,
                         reorder(modelo,auc, median))

mediasver2$accuracy <- 1-mediasver2$tasa

par(cex.axis=0.9,las=2)
boxplot(data=mediasver2,auc~modelo,col="#FF8D63",main='AUC')
boxplot(data=mediasver2,accuracy~modelo,col="#FF8D63",main='Accuracy')

listacontraste1 <- c("gbm", "predi18")

datacontraste<-medias0[which(medias0$modelo%in%listacontraste1),]
datacontraste$accuracy <- 1-datacontraste$tasa

# Para Accuracy
t.test(datacontraste$accuracy ~datacontraste$modelo)
# Para auc
t.test(datacontraste$auc ~datacontraste$modelo)

listacontraste2<-c("svmPoly","predi18")

datacontraste2<-medias0[which(medias0$modelo%in%listacontraste2),]
datacontraste2$accuracy <- 1-datacontraste2$tasa
# Para Accuracy
t.test(datacontraste2$accuracy ~datacontraste2$modelo)
# Para auc
t.test(datacontraste2$auc ~datacontraste2$modelo)

listacontraste3<-c("logi","predi18")

datacontraste3<-medias0[which(medias0$modelo%in%listacontraste3),]
datacontraste3$accuracy <- 1-datacontraste3$tasa
# Para Accuracy
t.test(datacontraste3$accuracy ~datacontraste3$modelo)
# Para auc
t.test(datacontraste3$auc ~datacontraste3$modelo)



library(caret)

testing <- archivo1 |>
  filter(role=="test") |>
  dplyr::select(c("id", "tpondgenre", "speechiness", "popularity",
                  "explicit","duration_min", "valence", "liked"))

data_test <- testing[order(testing$id),]

# Validación cruzada repetida
set.seed(1234)
control<-trainControl(method = "cv",number=10,
                      savePredictions = "all",classProbs=TRUE)

gbmgrid<-expand.grid(shrinkage=c(0.01),
                     n.minobsinnode=c(50),
                     n.trees=c(1000),
                     interaction.depth=c(2))
gbm<- train(factor(liked)~.,data=train,
            method="gbm",trControl=control,tuneGrid=gbmgrid,
            distribution="bernoulli", bag.fraction=1,verbose=FALSE)

pred_gbm<-predict(gbm,data_test, type = "prob")
names(pred_gbm) <- paste("pred_gbm", names(pred_gbm), sep = ".")

set.seed(1234)
control<-trainControl(method = "cv",number=10,
                      savePredictions = "all",classProbs=TRUE)
svmgrid<-expand.grid(C=c(0.01),
                     degree=c(3),scale=c(0.1))

svm<- train(factor(liked)~.,data=train,
            method="svmPoly",trControl=control,
            tuneGrid=svmgrid,verbose=FALSE)
pred_svm<-predict(svm,data_test, type = "prob")
names(pred_svm) <- paste("pred_svm", names(pred_svm), sep = ".")

predicciones <- cbind(pred_gbm,pred_svm)

predicciones$pred_ens <- apply(predicciones[,c("pred_gbm.Yes", "pred_svm.Yes")],1,mean)

test <- cbind(spotify_test, predicciones$pred_ens) |> dplyr::rename("prob" = "predicciones$pred_ens")

test$pred <- if_else(test$prob>=0.5, "Yes", "No")
test <- test |> mutate(liked=as.factor(liked),
               pred=as.factor(pred))

# MEDIDAS CON PUNTO DE CORTE 0.5

confusionMatrix(reference=test$liked,data=test$pred, positive="Yes")

# Cambiando el punto de corte

corte<-0.7

test$predcorte<-ifelse(test$prob>corte,"Yes","No")
test$predcorte<-as.factor(test$predcorte)

confusionMatrix(reference=test$liked,data=test$predcorte, positive="Yes")

test |> filter(score == "9" & prob <= "0.4") |>
  dplyr::select("title", "artist", "score")

ggplot(test, aes(prob, score)) +
  geom_point(col="#FF8D63", size=1.75) +
  stat_smooth(method = "lm", col= "#47CBFF", se=FALSE) +
  scale_color_manual(values = TFM_palette3) +
  labs(x="Probabilidad de recomendar",
       y= "Puntuación")

spotify_recom <- read_csv("recommender_spotify.csv")

test_recom <- test |> left_join(spotify_recom, by = join_by(title))
test_spot <- test_recom |> filter(recommender=="Spotify")
test_spot |> count(liked) |> mutate(porc=100*n/sum(n))
confusionMatrix(reference=test_spot$liked,data=test_spot$pred, positive="Yes")

control<-trainControl(method = "cv",number=4,savePredictions = "all",classProbs=TRUE)

logi_test<- train(factor(liked) ~ tpondgenre + speechiness + popularity +
               explicit + duration_min + valence,
             data=training,method="glm",trControl=control)
summary(logi_test)

test_graf <- cbind(data_test, predicciones$pred_ens) |> dplyr::rename("prob" = "predicciones$pred_ens")

lista_var <- c("tpondgenre", "speechiness", "popularity",
               "explicit","duration_min", "valence")
vtest<-list()
i=1
for (vari in lista_var){
  a<-ggplot(test_graf, aes_string(vari, "prob")) +
    geom_point(aes(fill = liked, color = liked)) +
    scale_fill_manual(values=TFM_palette3) +
    scale_color_manual(values=TFM_palette3) +
    stat_smooth(method = "lm", se = FALSE, color = "#E4A7F4") +
    if(i%%3!=0){guides(fill="none", color = "none")}
  # print(a)
  vtest[[i]]<-a
  i=i+1
}
png(file="./figuras/vtest.png",res=80,width=800,height=600)
grid.arrange(grobs=vtest,ncol=3,nrow=2, widths = c(1, 1, 1.25))
dev.off()

rfgrid<-expand.grid(mtry=c(6))
set.seed(12345)
control<-trainControl(method = "cv",number=4,savePredictions = "all")
rf<- train(factor(liked)~ tpondgenre + speechiness + popularity +
             explicit + duration_min + valence,data=testing,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=300,nodesize=10,replace=TRUE,
           importance=TRUE)

rf
# CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseGini),]
tabla2 <- tabla |> mutate(variable = rownames(tabla))
ggplot(tabla2, aes(x=reorder(variable, -MeanDecreaseGini), MeanDecreaseGini)) +
  geom_col(color = "#FF8D63", fill = "#FF8D63") +
  labs(x="Variable") +
  theme_minimal()

library(rpart)
library(rpart.plot)
arbol1 <- rpart(factor(liked) ~ tpondgenre + speechiness + popularity +
                  explicit + duration_min + valence, data = testing,
                minbucket = 3,method = "class",parms=list(split="gini"),cp=0)
rpart.plot(arbol1,extra=105, nn=TRUE, tweak = 1.2)
  
  #SAS
library(foreign)
write.dbf(testing,"C:/Users/charl/Documents/CloudStation/Uni/Master/TFM/testing.dbf")

sas <- read.csv("puntuacion_sas_SCORE.csv")
sas$liked <- as.factor(sas$liked)
sas$U_liked <- as.factor(sas$U_liked)
confusionMatrix(reference = sas$liked, data = sas$U_liked, positive = "Yes")

