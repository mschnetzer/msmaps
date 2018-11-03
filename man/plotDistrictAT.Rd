\name{plotDistrictAT}
\alias{plotDistrictAT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Austrian districts
}
\description{
Create map for Austrian districts (status 2017) with custom labels.
}
\usage{
plotDistrictAT(dataset, fillvar, colpal, ownlabs = F, citylabs = F, wienbezirke = F, tit, subtit, captit, savfile, legpos = "none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
Dataframe (must include \code{iso} for geographic identification)
}
  \item{fillvar}{
Variable of interest (VOI) included in the dataset
}
  \item{colpal}{
Color palette for discrete VOI, vector of min and max color for continuous VOI
}
  \item{ownlabs}{
Add own labels with dataframe called \code{ownlabels(iso, nudgex, nudgey, labname)}, \code{default=FALSE}
}
  \item{citylabs}{
Labels of regional capital cities, \code{default=FALSE}
}
  \item{wienbezirke}{
Zoom in and show districts of Vienna separately \code{default=FALSE}
}
  \item{legpos}{
Position of legend, \code{default="none"}
}
}
