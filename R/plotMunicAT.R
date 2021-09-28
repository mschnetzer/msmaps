#' Plots map for Austrian municipalities
#'
#' The map for Austrian municipalities is gratefully taken from https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/ under CC license.
#'
#' @param
#' dataset Dataframe (must include 'iso' for geographic identification)
#' fillvar Variable of interest (VOI)
#' colpal Colors palette for discrete VOI, Low/High for continuous VOI
#' ownlabs Add own dataframe \code{ownlabels(iso, nudgex, nudgey, labname, stringasfactors=F)}, default=FALSE
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

geodat <- gemeinden

if(citylabs==T) {
  labels <- data.frame(iso = c(80207,70101,50101,40101,30201,10101,60101,20101,90001),
                       nudgey=c(0.4,-0.5,0.1,0.5,1.2,-0.5,-1.5,-1.7,0.1),
                       nudgex=c(0,0.2,-0.6,0,0.6,1,0,-0.8,0.9))
  geodat <- geodat %>% left_join(labels,by="iso") %>%
  mutate(labselect=as.logical(iso %in% labels$iso))
} else {
  geodat <- geodat %>% mutate(labselect=NA)
}

if(ownlabs==T){
  geodat <- geodat %>% left_join(ownlabels,by="iso") %>%
    mutate(labselect=as.logical(iso %in% ownlabels$iso)) %>%
    mutate(name = case_when(labselect==T ~ labname, TRUE ~ name))
}

df <- geodat %>% left_join(dataset,by="iso")

plot <-
  ggplot(df) +
  geom_sf(aes(fill=get(paste(fillvar))),color="black",size=0.07) +
  coord_sf(datum=NA) +
  geom_label_repel(data=subset(df,labselect==TRUE),
                   aes(x = ccordx,y = ccordy,label = name),
                   color="black",size=2.5,nudge_y=df$nudgey[df$labselect==TRUE],
                   nudge_x=df$nudgex[df$labselect==TRUE],segment.size = 0.3) +
  theme_ms() + theme(legend.title = element_blank(), legend.position = legpos) +
  labs(x="",y="",title=tit,subtitle=subtit,caption=captit)

if(is.factor(df[[fillvar]])==T) {
  plot + scale_fill_manual(values=colpal)
  ggsave(savfile)
} else {
  plot + scale_fill_gradient(low=colpal[1],high=colpal[2])
  ggsave(savfile)
}
}
