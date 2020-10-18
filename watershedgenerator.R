#####################################
#### 1 Limpar Console e Memória #####
#####################################

gc(TRUE) #garbage colector da RAM
rm(list = ls()) #limpar memoria das variáveis globais 
dev.off() # limpar os plots
cat("\014") #limpar console

################################################################################
##### 2 Carregar bibliotecas, arquivos externos e definir pasta de trabalho ####
################################################################################

list.of.packages <- c("colorRamps","ggplot2","zoo","RColorBrewer", "ggrepel", "sf", "rgeos","ggforce",
                      "rworldmap", "rworldxtra","scales","openair","raster","rgdal","rasterVis",
                      "ggspatial","reshape2", "cowplot", "googleway", "networkD3","tidyverse") # lista de pacotes utilizados

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # checar se há algum pacote novo

if(length(new.packages)) install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, require, character.only = TRUE) # carrega os pacotes necessários

setwd("G:/My Drive/Pesquisa/Impressão Bacias Hidrográficas") #define a pasta de trabalho

####################################
####   3 Importanto os dados    ####
####################################

sr<- "+proj=utm +zone=24 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

mdepb<- raster("MDE_PB_SRTM_30m.tif") # carrega banda específica

mdepb<- projectRaster(mdepb, crs = sr)

shpdir <- list.files(getwd(),pattern = "*.shp",full.names = T) #pega o diretório de cada shape

shplist<- lapply(shpdir, function(x) readOGR(x,use_iconv = TRUE, encoding = "UTF-8")) # lista com os shapefiles

shplist <- lapply(shplist, function(x) spTransform(x, crs(mdepb)))

shplistnames<- gsub("G:/My Drive/Pesquisa/Impressão Bacias Hidrográficas/","",shpdir) #cria uma variável com os nomes dos shapes

names(shplist)<- shplistnames # da os nomes dos shapes para a lista de shapes

####################################
#### 4 Pré tratamento dos dados ####
####################################

### 4.1 Cortando o MDE pelas bacias

mdecrp <- list()

mdemask<- list()

n<- length(shplist$sub_bacias_pb.shp) 

for (i in 1:n) {
  
  mdecrp[[i]]<- crop(mdepb, shplist$sub_bacias_pb.shp[i,])
  
}

for (i in 1:length(shplist$sub_bacias_pb.shp)) {
  
  mdemask[[i]]<- mask(mdecrp[[i]], shplist$sub_bacias_pb.shp[i,])
  
}


#########################
#### 5 Gerando 3D ####
#########################

### 5.1 Transformar os arquivos para plotagem no ggplot

elmat<- list()
ambmat<- list()

for (i in 1:1) {
# for (i in 1:length(shplist$sub_bacias_pb.shp)) {

elmat[[1]] = matrix(raster::extract(mdemask[[1]], raster::extent(mdemask[[1]]), buffer = 1000),
               nrow = ncol(mdemask[[1]]), ncol = nrow(mdemask[[1]]))

ambmat[[1]] = ambient_shade(elmat[[1]])

maxw<- ((ncol(mdemask[[1]])*res(mdemask[[1]])[2])*(1/4000000))*1000

elmat[[1]] %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat[[1]]), color = "desert") %>%
  add_shadow(ray_shade(elmat[[1]], zscale = 3, maxsearch = 300), 0.5) %>%
  add_shadow(ambmat[[1]], 0.5) %>%
  plot_3d(elmat[[1]], zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
# render_snapshot()
save_3dprint(gsub(" ", "", paste(shplist$sub_bacias_pb.shp[1,]$Nome," ",".stl"), fixed = TRUE), 
             maxwidth = maxw, clear = TRUE)

}
