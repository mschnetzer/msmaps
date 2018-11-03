#' Plots map for Austrian municipalities
#'
#' The map for Austrian municipalities is gratefully taken from https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/ under CC license.
#'
#' @param
#' dataset Dataframe (must include 'iso' for geographic identification)
#' fillvar Variable of interest (VOI)
#' colpal Colors palette for discrete VOI, Low/High for continuous VOI
#' ownlabs Add own dataframe labels(iso, nudgex, nudgey, labname, stringasfactors=F), default=FALSE
#' citylabs Automatic regional capital labels, default=FALSE
#' legpos Position of legend, default="none"
#' tit Title of plot
#' subtit Subtitle of plot
#' captit Caption of plot
#' savfile Filename for saving
#' @export

plotMunicAT <- function(dataset,fillvar,colpal,ownlabs=F,citylabs=F,tit,subtit,captit,savfile,legpos="none"){

require(sf)
require(tidyverse)
require(ggrepel)
require(msthemes)

geodat <- st_read("data/gemeinden_999_geo.json", quiet=TRUE, stringsAsFactors=FALSE) %>%
  mutate(
    center = map(geometry, st_centroid),
    centercoord = map(center, st_coordinates),
    ccordx = map_dbl(centercoord, 1),
    ccordy = map_dbl(centercoord, 2)
    )

df %>% left_join(geodat, dataset, by="iso")

plot <-
  ggplot(df) +
  geom_sf(aes(fill=get(paste(fillvar))), color="black", size=0.07) +
  coord_sf(datum=NA) +
  scale_fill_gradient(low="white",high="#6d0202") +
  geom_label_repel(data=labels,
                   aes(x = ccordx,y = ccordy,label = name),
                   color="black",size=2.5,nudge_x=labels$nudgex,
                   nudge_y=labels$nudgey,segment.size = 0.3) +
  theme_ms() + theme(legend.title = element_blank(), legend.position = legpos) +
  labs(x="",y="",title=tit,subtitle=subtit,caption=captit) +
  ggsave(savfile,width=7,height=5,dpi=300)
}
