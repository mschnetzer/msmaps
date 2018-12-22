#' Plots map for Austrian states
#'
#' The map for Austrian states is gratefully taken from https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/ under CC license.
#'
#' @param
#' dataset Dataframe (must include 'iso' for geographic identification)
#' fillvar Variable of interest (VOI)
#' colpal Colors palette for discrete VOI, Low/High for continuous VOI
#' legpos Position of legend, \code{default="none"}
#' tit Title of plot
#' subtit Subtitle of plot
#' captit Caption of plot
#' savfile Filename for saving
#' @export

plotStatesAT <- function(dataset,fillvar,colpal,tit,subtit,captit,savfile,legpos="none"){

require(sf)
require(tidyverse)
require(ggrepel)
require(msthemes)

geodat <- laender

df <- geodat %>% left_join(dataset,by="iso")

plot <-
  ggplot(df) +
  geom_sf(aes(fill=get(paste(fillvar))),color="black",size=0.1) +
  coord_sf(datum=NA) + # datum=NA to supress long and lat labels
  theme_ms(alttf = F) + theme(legend.title = element_blank(), legend.position = legpos) +
  labs(x="",y="",title=tit,subtitle=subtit,caption=captit)

if(is.factor(df[[fillvar]])==T) {
  plot + scale_fill_manual(values=colpal) + ggsave(savfile)
} else {
  plot + scale_fill_gradient(low=colpal[1],high=colpal[2]) + ggsave(savfile)
}
}
