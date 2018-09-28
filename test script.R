library(assertthat)
library(rlang)
library(readr)

setwd('~/Reneal Dashboards')
data_files <- list.files(path = './development/2.1.3/deployment/data', pattern = '\\.rds$')

cnt <- 1

for(i in data_files){
        print(cnt)
        print(are_equal(read_rds(paste0('./email/2.1.3/deployment/data/', i)),
                  read_rds(paste0('./development/2.1.3/deployment/data/', i))))
        cnt <- cnt + 1
}
