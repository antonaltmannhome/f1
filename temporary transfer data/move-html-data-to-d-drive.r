
for (yrtodo in 2010:2018) {
	YRDIRTOUSE=paste(USERPATH,'data/',yrtodo,sep='')
	HTMLYRDIRTOUSE = paste0('d:/f1 data/html/', yrtodo)
	dircheckmake(HTMLYRDIRTOUSE)

	covsh=read.csv(paste(YRDIRTOUSE,'/covsh_clean.csv',sep=''),as.is=T)
	HTMLRACEDIRTOUSE = paste(HTMLYRDIRTOUSE, covsh$racename,sep='/')
	for (i in HTMLRACEDIRTOUSE) dircheckmake(i)
	DRIVERRACEDIRTOUSE=paste(HTMLRACEDIRTOUSE,'/driverrace',sep='')
	for (i in DRIVERRACEDIRTOUSE) dircheckmake(i)
}

htmfile = c('classification.htm', 'entry.htm', 'pitstop.htm', 'qualifying.htm', 'startinggrid.htm')
for (ri in 1:nrace) {
	myracename = raceDF$racename[ri]
	myyr = substr(myracename, 1, 4)
	fromdir = paste0('c:/research/lapbylap/data/', myyr,'/',myracename,'/')
	laptimefile = paste0(fromdir, 'laptime.htm')
	fromfile = paste0(fromdir,htmfile)
	if (file.exists(laptimefile)) fromfile = c(fromfile, laptimefile)
	tofile = gsub('c:/research/lapbylap/data', 'd:/f1 data/html', fromfile)
	# lets so all of those
	for (fi in 1:length(fromfile)) {
		file.rename(fromfile[fi], tofile[fi])
	}
	# then there's all the drivers files
	driverfile = list.files(paste0(fromdir,'/driverrace/'))
	fromdriverfile = paste0(fromdir, 'driverrace/', driverfile)
	todriverfile = gsub('c:/research/lapbylap/data', 'd:/f1 data/html', fromdriverfile)
	for (di in 1:length(fromdriverfile)) {
		file.rename(fromdriverfile[di], todriverfile[di])
	}
	print(ri)
}
