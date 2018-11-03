# msmaps
Create custom maps for Europe and Austria (districts and municipalities)

## Installation

`devtools::install_github("mschnetzer/msmaps")`

## Dependencies

This package requires my custom ggplot2 theme. Please also install `devtools::install_github("mschnetzer/msthemes")`.

## Usage

There are three functions available. The Austrian maps are based on [this github repository](https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/) and delivered in the package.

The functions are `plotEurope`, `plotDistrictAT` (for Austrian districts) and `plotMunicAT` (for Austrian municipalities) and have options:

`dataset`     Dataframe (must include 'iso' for geographic identification)  
`fillvar`     Variable of interest (VOI)  
`colpal`      Colors palette for discrete VOI, Low/High for continuous VOI  
`ownlabs`     Add own labels with dataframe called `ownlabels(iso, nudgex, nudgey, labname)`: `default=FALSE`  
`citylabs`    Automatic regional capital labels (not for `plotEurope`): `default=FALSE`  
`wienbezirke` Show Vienna districts separately (only for `plotDistrictAT`): `default=FALSE`  
`legpos`      Position of legend (not for `plotEurope`): `default="none"`  
`tit`         Title of plot  
`subtit`      Subtitle of plot  
`captit`      Caption of plot  
`savfile`     Filename for saving  
