#' Plots map for Austrian districts
#'
#' The map for Austrian districts is gratefully taken from https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/ under CC license.
#'
#' @param
#' dataset Dataframe (must include 'iso' for geographic identification)
#' fillvar Variable of interest (VOI)
#' colpal Colors palette for discrete VOI, Low/High for continuous VOI
#' ownlabs Add own dataframe \code{ownlabels(iso, nudgex, nudgey, labname, stringasfactors=F)}, default=FALSE
#' citylabs Automatic regional capital labels, \code{default=FALSE}
#' wienbezirke Show Vienna districts separately, \code{default=FALSE}
#' legpos Position of legend, \code{default="none"}
#' tit Title of plot
#' subtit Subtitle of plot
#' captit Caption of plot
#' savfile Filename for saving
#' @export

plotDistrictAT <- function(dataset,fillvar,colpal,ownlabs=F,citylabs=F,wienbezirke=F,tit,subtit,captit,savfile,legpos="none"){

require(sf)
require(tidyverse)
require(ggrepel)
require(msthemes)

if(wienbezirke==T) {
  geodat <- bezirkewien
} else {
  geodat <- bezirke
}

if(citylabs==T) {
  labels <- data.frame(iso = c(802,701,501,401,302,101,601,201,900),
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
  geom_sf(aes(fill=get(paste(fillvar))),color="black",size=0.1) +
  coord_sf(datum=NA) + # datum=NA to supress long and lat labels
  geom_label_repel(data=subset(df,labselect==TRUE),aes(x = ccordx,y = ccordy,label = name),
                   color="black",size=2.5,nudge_y=df$nudgey[df$labselect==TRUE],
                   nudge_x=df$nudgex[df$labselect==TRUE],segment.size = 0.3) +
  theme_ms(alttf = F) + theme(legend.title = element_blank(), legend.position = legpos) +
  labs(x="",y="",title=tit,subtitle=subtit,caption=captit)

if(is.factor(df[[fillvar]])==T) {
  plot + scale_fill_manual(values=colpal)
  ggsave(savfile, width = 9, height = 5, dpi = 320)
} else {
  plot + scale_fill_gradient(low=colpal[1],high=colpal[2])
  ggsave(savfile, width = 9, height = 5, dpi = 320)
}
}
