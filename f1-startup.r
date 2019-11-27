source('c:/research/general_startup.r')

library(RODBC)
USERPATH = 'c:/research/f1/'
COREPATH = paste0(USERPATH, 'core/')
UPDATECODEPATH = paste0(USERPATH, 'update/')
MODELCODEPATH = paste0(USERPATH, 'model code/')
HTMLPATH = 'd:/dropbox/f1 data newworld/html/'
ANTONCOMPUTER = c('ANTONLAPTOP', 'ANTON-PCHD2')
thiscomputer = Sys.info()[['nodename']]
TEMPPATH = 'c:/temp/'
AHKPATH = 'c:/research/utils/autohotkeys/'
SQLFUNCTPATH = 'c:/research/utils/sql/'
RCPPPATH = paste0(MODELCODEPATH, 'rcpp/')
odbcConnection = odbcConnect('cleanf1')
mySqlDbName = 'cleanf1'
# source('c:/research/utils/sql/sqlfunct.r')

# source(paste(COREPATH,'data-funct.r',sep=''))

library(ggplot2)
library(tidyr)
library(readr)
library(extrafont)
library(dplyr)
if (length(fonts()) == 0) stop('New version of R it seems, you need to do options(warn=0), ignore errors, then font_import() and then loadfonts(device = "win")(http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html)\n')

setwd(USERPATH)

library(f1data)
library(f1admin)
source(paste0(SQLFUNCTPATH, 'sqlfunct.r'))
source(paste0(MODELCODEPATH, 'model-funct.r'))

options(warn = 2)

f1admin:::CheckDatabaseUpToDate()
