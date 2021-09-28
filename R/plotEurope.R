#' Plot map for European countries
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
  require(rnaturalearth)
  require(rnaturalearthdata)

  sf_use_s2(FALSE)

  europe <- ne_countries(scale = "medium", returnclass = "sf")

  europe <- st_transform(europe, crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs')

  df <- left_join(europe, dataset, by=c("iso_a2"="geo"))

  if(!missing(labeldf)) {
    df <- df %>% left_join(labeldf, by = c("iso_a2"="geo")) %>%
      mutate(labselect=as.logical(iso_a2 %in% labeldf$geo),
             name = case_when(labselect==T ~ labname,
                              TRUE ~ NA_character_))
  }

  plot <-
    ggplot() +
    geom_sf(data = df, fill = "antiquewhite1", size = 0, alpha = 0.8) +
    geom_sf(data = df, aes(fill = get(paste(fillvar))),size=0.3,alpha=0.8) +
    coord_sf(xlim = c(2500000, 6100000), ylim = c(1200000, 5500000),
             expand = FALSE, label_axes = "") +
    theme_ms() +
    theme(panel.grid.major = element_line(color = "gray60", linetype = "ff",
                                          size = 0.1),
          panel.background = element_rect(fill = "aliceblue"),
          legend.position = c(0.12,0.85),
          legend.background = element_rect(fill="white", size=0.2,
                                           linetype="solid"),
          legend.title = element_blank(),
          legend.text = element_text(size=8),
          legend.key.size = unit(0.5,"cm"),
          plot.caption = element_text(margin = margin(t = unit(1,"lines"))),
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
    plot
    ggsave(savfile,width=6,height=6,dpi=300)
  } else {
    plot
  }
}
