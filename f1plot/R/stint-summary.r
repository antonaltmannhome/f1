.HatchWetSafetyCar = function(wetOrSafetyCar, myRace, myundriv, mynlap, lbl) {
  if (wetOrSafetyCar == 'wet') {
    lbl$columnToHatch = lbl$isWet
    hatchColour = 'black'
    hatchDensity = 5
  }
  if (wetOrSafetyCar == 'safetyCar') {
    lbl$columnToHatch = lbl$isSafetyCar
    hatchColour = 'greenyellow'
    hatchDensity = 12
  }
  sax=which(lbl$race==myRace)
  if (all(lbl$columnToHatch[sax]==1)) {
    ### if the whole damn lot are wet, just do one big rectangle
    rect(1,0.5,mynlap,length(myundriv)+0.5,border=NA,density=hatchDensity,col=hatchColour)
  }
  # otherwise just hatch in the wet bits
  if (any(lbl$columnToHatch[sax]==1) & !all(lbl$columnToHatch[sax]==1)) {
    for (j in 1:length(myundriv)) {
      dum=lbl$columnToHatch[sax[lbl$driver[sax]==myundriv[j]]]
      dum2=rle(dum)
      dum2$start=c(1,cumsum(dum2$len)[-length(dum2$len)]+1)
      dum2$end=cumsum(dum2$len)
      for (k in which(dum2$val==1)) rect(dum2$start[k],j-0.5,dum2$end[k],j+0.5,density=hatchDensity,col=hatchColour)
    }
    ### this legend overflows, so can't really use
    # legend(71,4,legend='wet, not used in model',fill='black',col='black',density=30,cex=0.7)
  }
  return(NULL)
}

StintSummary=function(myRace, includeInterruptedStint = FALSE, focusDriverStint = NULL,
                      hatchwet = FALSE, hatchsafetycar = FALSE,
                      modifiedStintDF = NULL, toFile = FALSE) {
  # ugh, there must be a nicer way of doing this:
  # i want the user to have option of providing stintDF, for the alternative strategy thing
  # but don't want user to have to send stintDF every time they run the function
  # so if you don't provice modifiedStintDF, i assume you just use normal stintDF
  if (toFile) {
    fileout=MakeRaceFile(myRace=myRace,	myFile='stint-summary.png')
    png(filename = fileout, width = 700, height = 700)
  }
  if (is.null(modifiedStintDF)) {
    modifiedStintDF = stintDF
  }
	raceDF = f1data:::MakePrettyRaceLabel(raceDF)
	myundriv=unique(rddf$driver[rddf$race == myRace & !is.na(rddf$startingGrid)])
	mynlap=raceDF$nlap[raceDF$race==myRace]
	myuntyre=unique(lbl$tyre[lbl$race==myRace])
	myfont='Trebuchet MS'
	### put the driver in qualifying order i suggest
	myundriv=myundriv[order(-with(rddf[rddf$race==myRace,],as.numeric(startingGrid[match(myundriv,driver)])))]
	# make an empty plot firstly
	resetpar()
	par(mai=c(0.75,1.75,0.5,1),xpd=T,cex.axis=0.75,las=1,family=myfont,bty='n')
	plot(seq(1,mynlap,le=length(myundriv)),1:length(myundriv),
	pch=' ',xaxt='n',yaxt='n',xlab='',ylab='',mgp=c(3,0.5,0),
	     family=myfont,
	     main=paste('tyre strategies:',raceDF$prettyRace[raceDF$race==myRace]))
	### put axes in every ten laps and at the end
	dum=sort(unique(c((1:mynlap)[ ((1:mynlap)%%10)==0],mynlap)))
	### if the penultimate point is within 3 of total laps, get rid
	if (dum[length(dum-1)]>dum[length(dum)-4]) dum=dum[-(length(dum)-1)]
	axis(1,at=dum,padj=-1.5)
	mtext(side=1,line=1.5,'lap number')
	# now add driver's names
	if (is.null(focusDriverStint)) {
		myundrivcol = rep('black', length(myundriv))
	}
	if (!is.null(focusDriverStint)) {
		### but colour in focus driver and their teammate
		myundrivcol=rep('black',length(myundriv))
		myundrivcol[which(myundriv==focusDriverStint$driver)]='red'
		myteam=with(rddf,team[which(race == myRace & driver==focusDriverStint$driver)])
		myteammate=with(rddf,driver[which(race == myRace & team==myteam & driver!=focusDriverStint$driver)])
		if (length(myteammate)>0) myundrivcol[which(myundriv==myteammate)]='orange'
	}

	for (j in 1:length(myundriv)) {
		text(par('usr')[1],j,driverDF$longDriver[driverDF$driver==myundriv[j]],pos=2,offset=0,col=myundrivcol[j])
	}
	### ok something wrong there, we'll come back to that
	tyrecoldf=ReadF1Data(paste0(USERPATH, 'f1admin/tyre.csv'), 'tyre')
	for (j in 1:length(myundriv)) {
		sax=which(modifiedStintDF$race==myRace & modifiedStintDF$driver==myundriv[j])
		if (length(sax)>0) {
			for (k in 1:length(sax)) {
				rect(modifiedStintDF$startLap[sax[k]], (j-0.4), modifiedStintDF$endLap[sax[k]], j+0.4,
						col=tyrecoldf$colour[tyrecoldf$tyre==modifiedStintDF$tyre[sax[k]]])
			}
		}
	}
	mytyrecol=tyrecoldf$colour[match(myuntyre,tyrecoldf$tyre)]
	legend(mynlap+1,length(myundriv)/2,myuntyre,pch=22,pt.bg=mytyrecol,col='black',pt.cex=1.75,cex=0.75)
	### if there were any phases we've declared wet, highlight them on the graph

	if (hatchwet) {
	  .HatchWetSafetyCar('wet', myRace, myundriv, mynlap, lbl)
	}
	if (hatchsafetycar) {
	  .HatchWetSafetyCar('safetyCar', myRace, myundriv, mynlap, lbl)
	}

	if (includeInterruptedStint) {
		anyInterruptedStint = with(modifiedStintDF, sum(race == myRace & isMessy) > 0)
		if (anyInterruptedStint) {
			problemDF = modifiedStintDF %>%
							filter(race == myRace & isMessy) %>%
							select(driver, stint, endLap, isSafetyCar, isRed, stintRetired)
			### by default, go for blue for interrupted stints
			problemDF$colour='blue'
			### cyan for safety cars or red flags
			problemDF$colour[which(problemDF$isSafetyCar | problemDF$isRed)] = 'firebrick4'
			### grey for retirement
			problemDF$colour[which(problemDF$stintRetired)]='grey'
			### but highlight with a square this particular stint
			if (is.null(focusDriverStint)) {
				problemDF$pch = 16
			}
			if (!is.null(focusDriverStint)) {
				problemDF$isfocus = with(problemDF, driver == focusDriverStint$driver & stint == focusDriverStint$stint)
				problemDF$pch=with(problemDF, ifelse(!isfocus,16,17))
			}
			legend(mynlap+1, length(myundriv)/4, c('SC/red','retire','interrupt'), pch=16, col=c('firebrick4','grey','blue'), pt.cex=1.75, cex=0.75)
			if (nrow(problemDF)>0) {
				for (j in 1:nrow(problemDF)) {
					sax=which(myundriv==problemDF$driver[j])
					points(problemDF$endLap[j],sax,col=problemDF$colour[j],pch=problemDF$pch[j],cex=1.5)
				}
			}
			if (FALSE) {
			### if there were any phases we've declared wet, highlight them on the graph
			sax=which(lbl$race==myRace)
			if (all(lbl$isWet[sax]==1)) {
				### if the whole damn lot are wet, just do one big rectangle
				rect(1,0.5,mynlap,length(myundriv)+0.5,border=NA,density=5,col='black')
			}
			# otherwise just hatch in the wet bits
			if (any(lbl$isWet[sax]) & !all(lbl$isWet[sax])) {
				for (j in 1:length(myundriv)) {
					dum=lbl$isWet[sax[lbl$driver[sax]==myundriv[j]]]
					dum2=rle(dum)
					dum2$start=c(1,cumsum(dum2$len)[-length(dum2$len)]+1)
					dum2$end=cumsum(dum2$len)
					for (k in which(dum2$val==1)) rect(dum2$start[k],j-0.5,dum2$end[k],j+0.5,density=5,col='black')
				}
				### this legend overflows, so can't really use
				# legend(71,4,legend='wet, not used in model',fill='black',col='black',density=30,cex=0.7)
			}
			}
		}
	}

	if (toFile) {
    dev.off()
	}

	### clean up after ourselves
	resetpar()
}
