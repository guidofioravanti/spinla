rm(list=objects())
library("tidyverse")
library("SPEI")
library("climatologici")
library("sf")
library("regioniItalia")

list.files(pattern="^prcp.+csv$")->ffile
spi.ref.start<-1991
spi.ref.end<-2020
spi.scale<-3
MAX.NA<-5

purrr::map(ffile,.f=function(.x){
  
  str_remove(str_remove(.x,"^prcp."),"\\.serie_valide.csv$")->regione
  
  read_delim(.x,delim=";",col_names = TRUE,col_types = cols(yy=col_integer(),mm=col_integer(),dd=col_integer(),.default = col_double())) %>%
    filter(yy>=1980 & yy<=2021) %>%
    dplyr::select(yy,mm,dd,everything())->dati 
  
  paste(regione,names(dati)[!names(dati) %in% c("yy","mm","dd")],sep="_")->names(dati)[!names(dati) %in% c("yy","mm","dd")]
  
  ClimateData(dati,param = "pr")->cdati
  climatologici::aggregaCD(cdati,max.na = MAX.NA,rle.check = TRUE,max.size.block.na = MAX.NA)->mdati
  
  #eliminiamo le colonne per cui non posso calcolare il climatologico annuale della precipitazione sul periodo 1981-2010
  climatologici::climatologiciMensili(mdati,yearS = 1981,yearE = 2010,max.na = 6,rle.check = TRUE,max.size.block.na = 6)->mclimatol
  climatologicoAnnuale(mclimatol)->yclimatol
  purrr::map_lgl(yclimatol %>% dplyr::select(-yy),.f=\(.x) (is.na(.x) |is.infinite(.x)))->colonneDaEliminare2
  mdati[,names(colonneDaEliminare2[!colonneDaEliminare2])]->mdati
  
  #eliminiamo le colonne per cui non posso calcolare il climatologico annuale della precipitazione sul periodo 1991-2020
  climatologici::climatologiciMensili(mdati,yearS = 1991,yearE = 2020,max.na = 6,rle.check = TRUE,max.size.block.na = 6)->mclimatol
  climatologicoAnnuale(mclimatol)->yclimatol
  purrr::map_lgl(yclimatol %>% dplyr::select(-yy),.f=\(.x) (is.na(.x) |is.infinite(.x)))->colonneDaEliminare2
  mdati[,names(colonneDaEliminare2[!colonneDaEliminare2])]->mdati
  
  as.data.frame(mdati)->dati2
  
  dati2 %>%
    dplyr::select(yy,mm)->yymm
  
  dati2 %>%
    dplyr::select(-yy,-mm)->seriePerSPI
  
  purrr::map_dfc(1:ncol(seriePerSPI),.f=function(.x){
    
    #if(all(is.na(seriePerSPI[[.x]]))) return()
    ts(seriePerSPI[[.x]],frequency = 12,start=1980)->myts
    
    spi(myts,scale = spi.scale,na.rm=TRUE,ref.start =c(spi.ref.start,1),ref.end=c(spi.ref.end,12))->ts_spi
    
    data.frame(as.vector(ts_spi$fitted))->out
    
    names(out)<-names(seriePerSPI)[.x]
    
    out
    
  })->dfSPI
  
  
  #elimino le colonne con solo NA
  purrr::map_lgl(dfSPI,.f=\(.x) all(is.na(.x)))->colonneDaEliminare
  dfSPI[,!colonneDaEliminare]  ->dfSPI
  bind_cols(yymm,dfSPI)->dfSPI

  
}) %>% reduce(.,.f=left_join,by=c("yy","mm"))->dfOut

tidyr::pivot_longer(dfOut,cols=matches("^[a-z]+_[0-9]+$"),names_to="SpiID",values_to = "spi") %>%
  filter(yy>=1981)->dfSPI

unique(dfSPI$SpiID)->codici

#anagrafica
read_delim("anagrafica.prcp.csv",delim=";",col_names = TRUE) %>%
  mutate(SpiID=paste(regione,SiteID,sep="_"))->ana

ana %>%
  filter(SpiID %in% codici)->ana

st_as_sf(ana,coords = c("Longitude","Latitude"),crs=4326)->sfAna

st_union(liguria,emiliaromagna)->nord
st_union(nord,lombardia)->nord
st_union(nord,veneto)->nord
st_union(nord,friuliveneziagiulia)->nord
st_union(nord,piemonte)->nord
st_union(nord,trentino)->nord
st_union(nord,valleaosta)->nord
st_transform(nord,crs=32632)->nord32632
st_buffer(nord32632,dist = 10000)->nord32632
st_transform(nord32632,crs=4326)->nord_buffered
st_transform(nord,crs=4326)->nord4326
#points within the nord buffered

st_join(sfAna,nord_buffered,join=st_within) %>%
  filter(!is.na(COD_REG))->sfAna_spi

png(glue::glue("map_spi{spi.scale}_{spi.ref.start}_{spi.ref.end}.png"),width=600,height=600)
plot(st_geometry(nord_buffered))
plot(st_geometry(nord4326),add=T)
plot(st_geometry(sfAna_spi),add=TRUE)
dev.off()

write_delim(ana %>% filter(SpiID %in% sfAna_spi$SpiID),delim=";",col_names = TRUE,file = glue::glue("ana_spi{spi.scale}_{spi.ref.start}_{spi.ref.end}.csv"))
write_delim(dfSPI %>% filter(SpiID %in% sfAna_spi$SpiID),delim=";",col_names = TRUE,file = glue::glue("spi{spi.scale}_{spi.ref.start}_{spi.ref.end}.csv"))

print(nrow(ana %>% filter(SpiID %in% sfAna_spi$SpiID)))