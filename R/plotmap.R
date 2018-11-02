##########################
### AUSTRIAN DISTRICTS ###
##########################

plotbezirke <- function(dataset,fillvar,colpal,ownlabs=F,citylabs=F,wienbezirke=F,tit,subtit,captit,savfile,legpos="none"){

# dataset = dataframe with VOI and 'iso' for geographic identification
# fillvar = VOI
# colpal = Colors for discrete VOI, Low/High for continuous VOI
# ownlabs = Add own labels as dataframe with (iso,nudgex,nudgey,labname, stringasfactors=F)
# citylabs = Automatic regional capital labels
# wienbezirke = Show Vienna districts
# legpos = Position of legend

require(sf)
require(tidyverse)
require(ggrepel)
require(msthemes)

# Load JSON map from https://github.com/ginseng666/GeoJSON-TopoJSON-Austria
map <- ifelse(wienbezirke==T, "~/Daten/Datasets/Maps/bezirke_wien_gross_geo.json","~/Daten/Datasets/Maps/bezirke_999_geo.json")  

geodat <- st_read(map, quiet=TRUE, stringsAsFactors=FALSE) %>%
  mutate(
    center = map(geometry, st_centroid),
    centercoord = map(center, st_coordinates),
    ccordx = map_dbl(centercoord, 1),
    ccordy = map_dbl(centercoord, 2)
  ) %>%
    mutate(iso = as.numeric(iso)) %>%
    mutate(name = str_replace_all(name,c("\\(Stadt\\)"="","-Stadt"=""," Stadt"=""," am Wörthersee"="")))

if(citylabs==T) {
  labels <- data.frame(iso = c(802,701,501,401,302,101,601,201,900),
                       nudgey=c(0.4,-0.5,0.1,0.5,1.2,-0.5,-1.5,-1.7,0.1),
                       nudgex=c(0,0.2,-0.6,0,0.6,1,0,-0.8,0.9))
}

df <- geodat %>% left_join(labels,by="iso") %>% left_join(dataset,by="iso") %>%
  mutate(labselect=as.logical(iso %in% labels$iso)) 

if(ownlabs==T){
  df <- df %>% mutate(name = case_when(labselect==T ~ labname, TRUE ~ name))
}

plot <-
  ggplot(df) +
  geom_sf(aes(fill=get(paste(fillvar))),color="black",size=0.1) +
  coord_sf(datum=NA) + # datum=NA to supress long and lat labels
  geom_label_repel(data=subset(df,labselect==TRUE),aes(x = ccordx,y = ccordy,label = name),
                   color="black",size=2.5,nudge_y=df$nudgey[df$labselect==TRUE],
                   nudge_x=df$nudgex[df$labselect==TRUE],segment.size = 0.3) +
  theme_ms(alttf = F) + theme(legend.title = element_blank(), legend.position = legpos) +
  labs(x="",y="",title=tit,subtitle=subtit,caption=captit) 

if(is.factor(df[[fillvar]])==T) {
  plot + scale_fill_manual(values=colpal) + ggsave(savfile)
} else {
  plot + scale_fill_gradient(low=colpal[1],high=colpal[2]) + ggsave(savfile)
}
}


#####################
### MAP OF EUROPE ###
#####################

ploteurope <- function(dataset,fillvar,colpal,tit,subtit,captit,savfile){
  
# dataset = dataframe with VOI and 'geo' with 2-digit country codes
# fillvar = VOI
# colpal = Colors for discrete VOI, Low/High for continuous VOI  
  
require(sf)
require(tidyverse)
require(msthemes)
library(rworldmap)
library(rworldxtra)
library(ggspatial)
library(rgdal)

world <- getMap(resolution = "high")
world <- world[which(world$REGION=="Europe" & world$NAME!="Greenland"),]
world <- spTransform(world, CRS=CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
world <- st_as_sf(world)

df <- left_join(world, dataset, by=c("ISO_A2"="geo"))

plot <-
  ggplot(data = df) +
  geom_sf(aes(fill = get(paste(fillvar))),size=0.3,alpha=0.8) +
  coord_sf(xlim = c(2500000, 6100000), ylim = c(1200000, 5500000), expand = FALSE, label_axes = "") +
  annotation_scale(location = "bl", width_hint = 0.4,height = unit(0.1,"cm"),text_cex = 0.5) +
  theme_ms() + 
  theme(panel.grid.major = element_line(color = gray(.1),linetype = "dashed", size = 0.1),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.12,0.85),
        legend.background = element_rect(fill="white", size=0.2, linetype="solid"),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.5,"cm")) +
  labs(title=tit,subtitle=subtit, caption=captit) 

if(is.factor(df[[fillvar]])==T) {
  plot + scale_fill_manual(values=colpal,na.value="antiquewhite1")
} else {
  plot + scale_fill_gradient(low=colpal[1],high=colpal[2],na.value="antiquewhite1")
}
plot + ggsave(savfile,width=6,height=6,dpi=300)
}
