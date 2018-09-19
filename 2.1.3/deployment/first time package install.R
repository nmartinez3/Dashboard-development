###1/19/2018
###added 'remove.packages()' function to clean install all packages

#run this to install required packages if they haven't been installed yet. if they've been installed already, no need to run this again
#installing is different than loading. the other scripts take care of loading, so you only need to run this once on a new computer

#installing packages
packages<-c('dplyr','ggplot2','shiny','dbplyr','rstudioapi','lubridate','readr','stringr','tidyr','shinydashboard','rlang','DT')
suppressWarnings(remove.packages(packages))
install.packages(packages)

#all set!