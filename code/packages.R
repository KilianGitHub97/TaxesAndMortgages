##### Packages #####
####################

pkgs<-c(
  "data.table",
  "tidyverse",
  "cluster",
  "mice",
  "VIM"
)
lapply(pkgs, library, character.only=TRUE)
