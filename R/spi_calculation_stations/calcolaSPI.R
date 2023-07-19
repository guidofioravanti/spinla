rm(list=objects())
library("tidyverse")
library("SPEI")
library("climatologici")
library("sf")
library("regioniItalia")

list.files(pattern="^prcp.+csv$")->ffile
spi.ref.start<-1981
spi.ref.end<-2010
spi.scale<-3
MAX.NA<-5

purrr::map(ffile,.f=function(.x){
  
  read_delim(.x,delim=";",col_names = TRUE,col_types = cols(yy=col_integer(),mm=col_integer(),dd=col_integer(),.default = col_double())) %>%
    filter(yy>=1981 & yy<=2021) %>%
    dplyr::select(yy,mm,dd,everything())->dati 
  
  ClimateData(dati,param = "pr")->cdati
  climatologici::aggregaCD(cdati,max.na = MAX.NA,rle.check = TRUE,max.size.block.na = MAX.NA)->mdati
  
  as.data.frame(mdati)->dati2
  dati2 %>%
    dplyr::select(yy,mm)->yymm
  
  dati2 %>%
    dplyr::select(-yy,-mm)->seriePerSPI
  
  purrr::map_dfc(1:ncol(seriePerSPI),.f=function(.x){
    
    #if(all(is.na(seriePerSPI[[.x]]))) return()
    ts(seriePerSPI[[.x]],frequency = 12,start=1981)->myts
    spi(myts,scale = spi.scale,na.rm=TRUE,ref.start =c(spi.ref.start,1),ref.end=c(spi.ref.end,12))->ts_spi
    as.vector(ts_spi$fitted)
    
  })->dfSPI
  

  
  names(dfSPI)<-names(seriePerSPI)
  #elimino le colonne con solo NA
  purrr::map_lgl(dfSPI,.f=\(.x) all(is.na(.x)))->colonneDaEliminare
  dfSPI[,!colonneDaEliminare]  ->dfSPI
  
  #eliminiamo le colonne per cui non posso calcolare il "climatologico" dell'SPI (ovvero quelle serie con troppi buchi nel periodo di riferimento per SPI)
  bind_cols(yymm,dfSPI)->dfSPI
  ClimateData(dfSPI,param = "pr")->mdati  
  climatologici::climatologiciMensili(mdati,yearS = spi.ref.start,yearE = spi.ref.end,max.na = 6,rle.check = TRUE,max.size.block.na = 6)->mclimatol
  climatologicoAnnuale(mclimatol)->yclimatol
  purrr::map_lgl(yclimatol %>% dplyr::select(-yy),.f=\(.x) (is.na(.x) |is.infinite(.x)))->colonneDaEliminare2
  print(.x)

  dfSPI[,names(colonneDaEliminare2[!colonneDaEliminare2])]  ->dfSPI
  bind_cols(yymm,dfSPI)
  
})%>% reduce(.,.f=left_join,by=c("yy","mm"))->dfOut

tidyr::pivot_longer(dfOut,cols=matches("[0-9]+"),names_to="SiteID",values_to = "spi")->dfSPI

unique(dfSPI$SiteID)->codici

#anagrafica
read_delim("anagrafica.prcp.csv",delim=";",col_names = TRUE)->ana
ana %>%
  filter(SiteID %in% codici)->ana

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

write_delim(ana %>% filter(SiteID %in% sfAna_spi$SiteID),delim=";",col_names = TRUE,file = glue::glue("ana_spi{spi.scale}_{spi.ref.start}_{spi.ref.end}.csv"))
write_delim(dfSPI %>% filter(SiteID %in% sfAna_spi$SiteID),delim=";",col_names = TRUE,file = glue::glue("spi{spi.scale}_{spi.ref.start}_{spi.ref.end}.csv"))


dfSPI %>% filter(SiteID %in% sfAna_spi$SiteID)->dfPlot
ggplot(data=dfPlot,aes(x=yy,y=spi))+
  geom_boxplot()+
  facet_wrap(~mm)
