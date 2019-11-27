### let's compare the revisit-blocked-laps-writeup-setup calculations to the standard ones. should we ditch current ones and replace them?

source('model code/model-startup.r')

if (FALSE) {
	# in old world:
	colToTransfer = c('rr', 'driver', 'lap', 'starttelapse', 'endtelapse', 'startaheaddriv', 'startaheadsec', 'startstarttgap', 'startupdatetgap', 'startaheadoutlier', 'endaheaddriv', 'endaheadsec', 'endstarttgap', 'endupdatetgap', 'endaheadoutlier', 'startlimlap', 'endlimlap', 'starttgap', 'updatetgap', 'isLeading')
	write.csv(file = 'c:/temp/temp.csv', lbl2[,colToTransfer], row.names = FALSE)
}

lbl2 = left_join(lbl,
				read.csv('c:/temp/temp.csv'),
				c('rr', 'driver', 'lap'))
				