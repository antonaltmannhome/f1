# try to read in the bare minimum to get as far as the first model

# let's make files which allow us to declare if we think a column is needed, see how much we can strip things down

racedbColumn = sqlQuery2('show columns from race')$Field
racedbColumn = tibble(column = racedbColumn, use = F)
racedrivColumn = sqlQuery2('show columns from racedriv')$Field
racedrivColumn = tibble(column = racedrivColumn, use = F)
lblColumn = sqlQuery2('show columns from lapbylap')$Field
lblColumn = tibble(column = lblColumn, use = F)
stintColumn = sqlQuery2('show columns from stint')$Field
stintColumn = tibble(column = stintColumn, use = F)
driverColumn = sqlQuery2('show columns from driver')$Field
driverColumn = tibble(column = driverColumn, use = F)

cleaningpath = paste(USERPATH, 'cleaning/', sep = '')
write.csv(file = paste(cleaningpath, 'race.csv', sep = ''), racedbColumn)
write.csv(file = paste(cleaningpath, 'racedriv.csv', sep = ''), racedrivColumn)
write.csv(file = paste(cleaningpath, 'lbl.csv', sep = ''), lblColumn)
write.csv(file = paste(cleaningpath, 'stint.csv', sep = ''), stintColumn)
write.csv(file = paste(cleaningpath, 'driver.csv', sep = ''), driverColumn)
