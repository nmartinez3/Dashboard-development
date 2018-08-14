#Hey David!
#run this once to install the required packages. if you've already run this, no need to run it again unless Rene says you need to

#installing packages
packages<-c('dplyr','ggplot2','shiny','dbplyr','DBI','RMySQL','rstudioapi','lubridate','readr','stringr','tidyr','shinydashboard','rlang','DT')
suppressWarnings(remove.packages(packages))
install.packages(packages)

#all set! please open server.R or ui.R and click "Run App"