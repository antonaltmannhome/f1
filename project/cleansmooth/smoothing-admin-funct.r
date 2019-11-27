
RetrieveMaxTheta = function(myRace, smoothParam, whatToSmooth) {
	
	smoothModelFile=MakeRaceFile(myRace, 'smooth-model.csv')

	isStandardModel = !'dCoefName' %in% names(whatToSmooth)
	
	if (!isStandardModel) {
		maxTheta = NULL
	}
	
	if (isStandardModel) {
		dum = MakeSmoothModelChoiceCombo(smoothParam, whatToSmooth, isStandardModel)
		smoothModelChoiceCombo = dum$smoothModelChoiceCombo
		smoothModelChoiceComboName = dum$smoothModelChoiceComboName

		
		### so, has this model already been run?
		if (!file.exists(smoothModelFile)) {
			alreadyDone = FALSE
			maxTheta = NULL
		}
		if (file.exists(smoothModelFile)) {

			allCombo=ReadF1Data(smoothModelFile, 'smoothing')
			allCombo$fileIsDone = !is.na(allCombo$sqDiff)

			allCombo = indicate_overlapping_combination(
						allCombo,
						smoothModelChoiceCombo,
						smoothModelChoiceComboName,
						'isCurrentModel')
			
			alreadyDone = with(allCombo, fileIsDone[isCurrentModel])
			if (!alreadyDone) {
				maxTheta = NULL
			}
			if (alreadyDone) {
				maxTheta=as.numeric(unlist(strsplit(as.character(with(allCombo, maxTheta[isCurrentModel])),split=',')))
			}
		}
	}
	
	return(maxTheta)
}

DisplayChoice = function(smoothParam, modelChoice, customSmoothInfo, myRace) {
	
	cat('# About to process the following combination:\n')
	DebugDisplay(smoothParam)
	message('myRace = "', myRace, '"')
	
	if (is.null(modelChoice)) {
		message('modelChoice = NULL')
	}	
	if (!is.null(modelChoice)) {
		cat('# model choice is:\n')
		message('modelChoice = ', modelChoice)
	}
	if (!is.null(customSmoothInfo)) {
		cat('# And smoothing the following objects:\n')
		dum = CorrectCharacterForDisplay(customSmoothInfo)
		message(paste0('customSmoothInfo = list(',paste(dum, collapse = ', '), ')'))
	}
	if (is.null(customSmoothInfo)) {
		message('customSmoothInfo = NULL')
	}
	cat('\n')
}

MakeSmoothModelChoiceCombo = function(smoothParam, whatToSmooth, isStandardModel) {

	smoothParamName = GetSmoothParamName()
	
	smoothModelChoiceComboList = smoothParam
	smoothModelChoiceComboName = smoothParamName
	if (isStandardModel) {
		# then it should be saved amongst our existing list - tack on the modelChoice
		smoothModelChoiceComboList$modelChoice = whatToSmooth$modelChoice
		smoothModelChoiceComboName = c(smoothModelChoiceComboName, 'modelChoice')
	}

	smoothModelChoiceCombo = purrr::map_df(smoothModelChoiceComboList, function(x) x)

	return(list(smoothModelChoiceCombo = smoothModelChoiceCombo,
				smoothModelChoiceComboName = smoothModelChoiceComboName))
}

CheckValidSmoothInfo = function(smoothParam, whatToSmooth) {
	
	isStandardModel = !'dCoefName' %in% names(whatToSmooth)
	dum = MakeSmoothModelChoiceCombo(smoothParam, whatToSmooth, isStandardModel)
	smoothModelChoiceCombo = dum$smoothModelChoiceCombo
	smoothModelChoiceComboName = dum$smoothModelChoiceComboName
	### check that you haven't chosen a nonsensical combination of choices
	allCombo=GenerateCombo(filterList=NULL)
	if (isStandardModel) {
		allRelevantCombo = allCombo
	}
	if (!isStandardModel) {
		# we can't check our chosen thing to smooth is right, because it's not supposed to be in allCombo
		allRelevantCombo = allCombo %>%
							select(-modelChoice) %>%
							distinct()
	}
		
	smoothModelChoiceCombo = indicate_overlapping_combination(
							smoothModelChoiceCombo, allRelevantCombo, smoothModelChoiceComboName, 'inAllCombo')
	if (!smoothModelChoiceCombo$inAllCombo) {
		stop('That is a strange combination of choices, assume it\'s a mistake, exiting\n')
	}

	if (!isStandardModel) {
		# got a few other ones to check
		if (smoothParam$useStretch & is.null(whatToSmooth$modelChoice)) {
			stop('You need to provide a modelChoice if you want to stretch with a non-standard model\n')
		}
		if (!smoothParam$qrToFit %in% c('q', 'r')) {
			stop('can not customise mixtures of qual and race yet\n')
		}
	}

	if (!smoothModelChoiceCombo$fwbw %in% c('bw','fwbw')) stop('fwbw has to be fwbw or bw\n')
	if (!smoothModelChoiceCombo$useStretch %in% c(FALSE,TRUE)) stop('useStretch has to be FALSE or TRUE\n')		
}

GenerateCombo = function(filterList) {
	### let's get in whatever we have done, don't complain about anything that's missing
	combolist = list(qrToFit = c('q', 'r', 'qr'),
					qrToPredict = c('q', 'r', 'rfinpos'),
					modelChoice = c('qual', 4, 30),
					useStretch = c(FALSE, TRUE),
					fwbw = c('bw', 'fwbw'))
	## however, there are some combinations that make no sense
	if (!is.null(filterList)) {
		for (j in 1:length(filterList)) {
			combolist[names(filterList)[j]] = filterList[j]
		}
	}
	allCombo = expand.grid(combolist,stringsAsFactors=F)
	### however, there are combinations within that that make no sense, so let's filter a little more
	getRid = NULL
	getRid[[1]] = with(allCombo, which(qrToFit == 'q' & qrToPredict %in% c('r', 'rfinpos')))
	getRid[[2]] = with(allCombo, which(qrToFit == 'r' & qrToPredict == 'q'))
	getRid[[3]] = with(allCombo, which(modelChoice == 'qual' &
										(qrToPredict %in% c('rfinpos', 'r') | qrToFit != 'q')))
	getRid = unique(do.call(c, getRid))
	if (length(getRid) > 0) allCombo = allCombo[-getRid,]
	rownames(allCombo) = 1:nrow(allCombo)
	return(allCombo)
}

GetSmoothParamName = function() {
	# very small function but total pain not having it
	smoothParamName=c('qrToFit', 'qrToPredict', 'useStretch', 'fwbw')
	return(smoothParamName)
}

# awful name, should be something like 'smoothing.storeallcoef'
RunModel=function(myRace, filterList=NULL, reset=F) {
	### have we already prepared a file for this? if not, make it now
	smoothModelFile = MakeRaceFile(myRace, 'smooth-model.csv')

	filterCombo = GenerateCombo(filterList = filterList)

	if (!file.exists(smoothModelFile)) {
		allCombo = GenerateCombo(filterList = NULL)

		allCombo$fileIsDone = FALSE
		allCombo[, c('maxTheta', 'smoothCoef', 'sqDiff')] = NA
	}
	
	if (file.exists(smoothModelFile)) {
		allCombo = ReadF1Data(smoothModelFile, 'smoothing')
		allCombo$fileIsDone = !is.na(allCombo$sqDiff)
	}
	
	## now filter down to the ones user has asked for (via filterList)
	smoothParamName = GetSmoothParamName()
	smoothModelChoiceComboName = c(smoothParamName, 'modelChoice')

	allCombo = indicate_overlapping_combination(
				allCombo,
				filterCombo,
				smoothModelChoiceComboName,
				'isInFilter')
				
	modelToRun = with(allCombo, which(isInFilter & !fileIsDone))
	
	if (length(modelToRun) == 0) {
		print('No new models to run, exiting\n')
		return(NULL)
	}
	if (length(modelToRun) > 0) {
		cat('Have got',length(modelToRun),'models to run...\n')
		for (j in modelToRun) {
			smoothOutput = GetSmooth(qrToFit = allCombo$qrToFit[j],
										qrToPredict = allCombo$qrToPredict[j],
										modelChoice = allCombo$modelChoice[j],
										useStretch = allCombo$useStretch[j],
										fwbw = allCombo$fwbw[j],
										myRace = myRace)
			allCombo$maxTheta[j] = paste(smoothOutput$optcoef, collapse = ', ')
			mycoef = InitialiseAndProcessCoef(theta = smoothOutput$optcoef,
											smoothParam = allCombo[j, smoothParamName])$mycoef
			### now fill out the data frame
			allCombo$smoothCoef[j] = paste(paste(names(mycoef), round(unlist(mycoef),6), sep = ': '),collapse = ', ')
			### we would also like the sqDiff
			allCombo$sqDiff[j] = smoothOutput$sqDiff
		}
	}
	# don't want to write anything that isn't either model parameters or maximum info
	maximumColumnName = c('sqDiff', 'smoothCoef', 'maxTheta')
	allCombo = allCombo %>%
				select(smoothModelChoiceComboName, maximumColumnName)
	write_csv(allCombo, path = smoothModelFile)
}
