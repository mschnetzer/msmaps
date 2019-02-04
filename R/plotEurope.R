#' Plot map for European countries
#'
#' The map for Europe is included in the R-packages rworldmap and rworldxtra
#'
#' @param
#' dataset Dataframe including variable 'geo' with 2-digit country codes
#' fillvar Variable of interest (VOI)
#' labeldf Add own dataframe \code{labeldf(iso, nudgex, nudgey, labname, stringasfactors=F)}
#' colpal Colors palette for discrete VOI, Low/High for continuous VOI
#' tit Title of plot
#' subtit Subtitle of plot
#' captit Caption of plot
#' savfile Filename for saving
#' @export

plotEurope <- function(dataset,fillvar,colpal,tit,subtit,captit,labeldf,savfile){

  require(sf)
  require(tidyverse)
  require(msthemes)
  library(rworldmap)
  library(rworldxtra)
  library(ggspatial)
  library(rgdal)

  world <- getMap(resolution = "high")
  world <- world[which(world$REGION=="Europe" & world$NAME!="Greenland"),]
  world <- spTransform(world, CRSobj = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  world <- st_as_sf(world)
  world$LON <- st_coordinates(st_centroid(world$geometry))[,1]
  world$LAT <- st_coordinates(st_centroid(world$geometry))[,2]


  df <- left_join(world, dataset, by=c("ISO_A2"="geo"))

  if(!missing(labeldf)) {
    df <- df %>% left_join(labeldf, by = c("ISO_A2"="geo")) %>%
      mutate(labselect=as.logical(ISO_A2 %in% labeldf$geo),
             name = case_when(labselect==T ~ labname,
                              TRUE ~ NA_character_))
  }

  plot <-
    ggplot() +
    geom_sf(data = df, fill = "antiquewhite1", size = 0, alpha = 0.8) +
    geom_sf(data = df, aes(fill = get(paste(fillvar))),size=0.3,alpha=0.8) +
    coord_sf(xlim = c(2500000, 6100000), ylim = c(1200000, 5500000),
             expand = FALSE, label_axes = "") +
    annotation_scale(location = "bl", width_hint = 0.4,
                     height = unit(0.1,"cm"),text_cex = 0.5) +
    theme_ms() +
    theme(panel.grid.major = element_line(color = gray(.1),
                                          linetype = "dashed", size = 0.1),
          panel.background = element_rect(fill = "aliceblue"),
          legend.position = c(0.12,0.85),
          legend.background = element_rect(fill="white", size=0.2,
                                           linetype="solid"),
          legend.title = element_blank(),
          legend.text = element_text(size=8),
          legend.key.size = unit(0.5,"cm"),
          axis.title = element_blank()) +
      labs(title=tit,subtitle=subtit,caption=captit)

  if(is.factor(df[[fillvar]])==T) {
    plot <- plot + scale_fill_manual(values=colpal,na.translate=FALSE)
  } else {
    plot <- plot + scale_fill_gradient(low=colpal[1],high=colpal[2],na.translate=FALSE)
  }

  if(!missing(labeldf)){
    plot <- plot +
      geom_label_repel(data=subset(df,labselect==TRUE),
            aes(x = LON, y = LAT, label = name),hjust=0.5,
            nudge_y=df$nudgey[df$labselect==TRUE]*10000,
            nudge_x=df$nudgex[df$labselect==TRUE]*10000,
            color="black",size=2.5,segment.size = 0)
  }

  if(!missing(savfile)) {
    plot + ggsave(savfile,width=6,height=6,dpi=300)
  } else {
    plot
  }
}
