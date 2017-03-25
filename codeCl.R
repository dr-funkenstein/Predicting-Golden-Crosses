company <- read.csv('companylist.csv', stringsAsFactors = FALSE)

cl <- makeCluster(getOption('cl.cores',16))

clusterFunction <- function(x ) {
	options('getSymbols.warnings4.0'=FALSE)
	setwd( "D:/Dropbox New/Dropbox/Documents/Master/Kod/FinalCode")
	source('./codeWindows.R')
	temp <- CreateCase(x, folder = './CasesBig6/')
	return( temp )
}

testCL <- parLapplyLB(cl, company, clusterFunction )