library(quantmod)
library(rugarch)
library(ggplot2)

CreateCase <- function(companyList, startdate = '1990-01-01', 
	enddate = '2016-12-31', folder = './Cases/', burn = 1000, 
	iterations = 1000){
	# Downloads data from Yahoo finance and performce simple manipulation of
	# data. Saves everything in a subfolder to folder named after the symbol. 

	symbol <- companyList$Symbol

	# Setting up the variables that are to be saved

	# Collecting data and renaming the columns
	print('Get data')
	fail <- FALSE
	tryCatch(timeseries <- get( getSymbols(symbol, src='yahoo', 
		from = startdate, to = enddate) ), 
		error = function(e){fail <<- TRUE } )
	if (fail) {
		return()
	} else {
		if (dim(timeseries)[1] < (burn + 100)  ){
			return()
		}
	}
	timeseries <- as.data.frame(timeseries)

	colnames(timeseries) <- c('Open', 'High', 'Low', 'Close',
		'Volume', 'Adjusted')

	index <- list()

	misc <- list()
	misc$symbol 	<- symbol
	misc$startdate 	<- row.names(as.matrix(timeseries))[1]
	misc$enddate 	<- tail(row.names(as.matrix(timeseries)), n = 1)
	misc$folder 	<- folder
	misc$datapoints <- dim(timeseries)[1]
	misc$alpha 		<- 2/51
	misc$beta 		<- 2/201
	misc$iterations <- iterations
	misc$burn 		<- burn
	#Comment out the data that you do not have here 
	misc$Name 		<- companyList$Name
	misc$MarketCap  <- companyList$MarketCap
	misc$Sector 	<- companyList$Sector
	misc$IPOyear	<- companyList$IPOyear
	misc$industry	<- companyList$industry
	# The primary analysis is done on the adjusted price

	timeseries$Price <- timeseries$Adjusted

	# Holds the change in different assets and other timeseries of 
	# length one less than the entire dataset. 
	diffTimeseries <- data.frame(Pricechange = c(diff(timeseries$Price)))
	print('ROI')
	# Return of investment 
	diffTimeseries$ROI <- diffTimeseries$Pricechange / 
		timeseries$Price[-misc$datapoints]

	print('EMA')
	# Exponential Moving Average
	ShortEMA <- LongEMA <- rep(0,misc$datapoints)
	for(i in 1:(misc$datapoints - 1)){
		ShortEMA[i+1] 	<- ShortEMA[i] * (1 - misc$alpha) + 
			timeseries$Price[i+1] * misc$alpha
		LongEMA[i+1] 	<- LongEMA[i] * (1 - misc$beta) + 
			timeseries$Price[i+1] * misc$beta 
	}
	timeseries$ShortEMA <- ShortEMA
	timeseries$LongEMA 	<- LongEMA
	timeseries$DiffEMA 	<- ShortEMA - LongEMA

	timeseries$ShortEMA[1:burn] <- timeseries$LongEMA[1:burn] <- 
		timeseries$DiffEMA[1:burn] <- NA

	print('Crossings')
	# Finding the index of the crossings
	index$Crossings 	<- which(diff(sign(timeseries$DiffEMA)) != 0)
	tempSign 			<- sign(timeseries$DiffEMA[index$Crossings])
	index$UpCrossings 	<- index$Crossings[tempSign < 0]
	index$DownCrossings <- index$Crossings[tempSign > 0]
	misc$NumberOfCrossings <- length(index$Crossings)

	# Estimating the variance and the GARCH parameters as functions of time
	print('Predicting variance')
	model = ugarchspec(variance.model = list(model = 'sGARCH', 
		garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0), 
		include.mean = FALSE))
	fit <- tryCatch(ugarchfit( model, diffTimeseries$ROI[1:burn] ), 
		warning = function(w) fail<<-TRUE)
	if(fail) {
		return()
 	}

	misc$a0 <- as.numeric(fit@fit$coef[1])
	misc$a1 <- as.numeric(fit@fit$coef[2])
	misc$b1 <- as.numeric(fit@fit$coef[3])

	# Calculate the variance backwards
	sigma2 <- array(0, dim = length(diffTimeseries$ROI))
	sigma2[1] <- misc$a0 + misc$a1 * diffTimeseries$ROI[1]^2 + misc$b1 * 
	var(diffTimeseries$ROI[1:burn])
	for (i in 2:(misc$datapoints - 1)) {
	sigma2[i] <- misc$a0 + misc$a1 * diffTimeseries$ROI[i - 1]^2 + 
		misc$b1 * sigma2[i - 1]
	}
	diffTimeseries$Sigma2 <- sigma2


	print('Estimating Probabilities')
	# Estimating The Probabilities 
	#--- Should be done here ----
	timeseries <- ProbEst(diffTimeseries, timeseries, misc)
	daysAhead <- c(1,2,5,10,20)


	# Creating folders.
	
	# creating subfolder if it does not exist
	dir.create(folder, showWarnings = FALSE) 
	dir.create(paste(folder, symbol, sep=''))

	print('Saving data')
	# Writing data to file
	write.table(timeseries, file = paste(folder, symbol, 
		'/timeseries.txt', sep=''), sep = ',')
	write.table(diffTimeseries, file = paste(folder, symbol, 
		'/difftimeseries.txt', sep=''), sep = ',')
	write.table(misc, file = paste(folder, 'misc.txt', sep=''), 
		sep = ',', append = TRUE, col.names = 
		!file.exists(paste(folder, 'misc.txt', sep='')), 
		row.names = FALSE)
	dump('index', file = paste(folder, symbol, '/index.R', sep=''))
	# Seperate up and down crossings and only write when available
	if(length(index$UpCrossings)){
		upDf <- as.data.frame( matrix(NA, nrow = length(index$UpCrossings), 
			ncol = 41))
		for (i in seq_along(index$UpCrossings)) {
			upDf[i, ] <- c(coredata(timeseries$Price[(index$UpCrossings[i]-20):
				(index$UpCrossings[i]+20)]))
		}
		upDf$Tick <- misc$symbol
		write.table(upDf, file = 
				paste(folder, 'UpCrossingsPrice.txt', sep = '/'), 
			sep = ',', append = TRUE, row.names = FALSE, 
			col.names = !file.exists(
				paste(folder, 'UpCrossingsPrice.txt', sep = '/')))
		for( day in daysAhead){
			upDf <- as.data.frame( matrix(NA, nrow = length(index$UpCrossings), 
				ncol = 31))
			for (i in seq_along(index$UpCrossings)) {
				upDf[i, ] <- c(timeseries[(index$UpCrossings[i]-30):
					(index$UpCrossings[i]), paste('ProbMonte' , day, sep = '') ])
			}
			upDf$Tick <- misc$symbol
			write.table(upDf, file = 
				paste(folder, 'UpProbMonte' , day,'.txt', sep = ''), 
				sep = ',', append = TRUE, row.names = FALSE, 
				col.names = !file.exists(
					paste(folder, 'UpProbMonte' , day,'.txt', sep = '')))
			upDf <- as.data.frame(matrix(NA, nrow = length(index$UpCrossings), 
				ncol = 31))
			for (i in seq_along(index$UpCrossings)) {
				upDf[i, ] <- c(timeseries[(index$UpCrossings[i]-30):
					(index$UpCrossings[i]), paste( 'ProbDirect' , day, sep = '') ])
			}
			upDf$Tick <- misc$symbol
			write.table(upDf, file = 
					paste(folder, 'UpProbDirect' , day,'.txt', sep = ''), 
				sep = ',', append = TRUE, row.names = FALSE, 
				col.names = !file.exists(
					paste(folder, 'UpProbDirect', day, '.txt', sep ='')))


		}

	}

	if(length(index$DownCrossings)){
		downDf <- as.data.frame(matrix(NA, nrow = length(index$DownCrossings), 
			ncol = 41))
		for (i in seq_along(index$DownCrossings)) {
			downDf[i, ] <- c(coredata(timeseries$Price[(index$DownCrossings[i]-20):
				(index$DownCrossings[i]+20)]))
		}
		downDf$Tick <- misc$symbol
		write.table(downDf, file = 
					paste(folder, 'DownCrossingsPrice.txt', sep = '/'), 
				sep = ',', append = TRUE, row.names = FALSE, 
				col.names = !file.exists(
					paste(folder, 'DownCrossingsPrice.txt', sep = '/')))
		for( day in daysAhead){
			
			downDf <- as.data.frame(matrix(NA, nrow = length(index$DownCrossings), 
				ncol = 31))
			for (i in seq_along(index$DownCrossings)) {
				downDf[i, ] <- c(coredata(timeseries[(index$DownCrossings[i]-30):
					(index$DownCrossings[i]), paste('ProbMonte', day, sep = '')]))
			}
			downDf$Tick <- misc$symbol
			write.table(downDf, file = 
					paste(folder, 'DownProbMonte' , day,'.txt', sep = ''), 
				sep = ',', append = TRUE, row.names = FALSE, 
				col.names = !file.exists(
					paste(folder, 'DownProbMonte' , day,'.txt', sep = '')))

			downDf <- as.data.frame(matrix(NA, nrow = length(index$DownCrossings), 
				ncol = 31))
			for (i in seq_along(index$DownCrossings)) {
				downDf[i, ] <- c(coredata(timeseries[(index$DownCrossings[i]-30):
					(index$DownCrossings[i]), paste('ProbDirect', day, sep = '')]))
			}
			downDf$Tick <- misc$symbol
			write.table(downDf, file = 
					paste(folder, 'DownProbDirect' , day,'.txt', sep = ''), 
				sep = ',', append = TRUE, row.names = FALSE, 
				col.names = !file.exists(
					paste(folder, 'DownProbDirect', day, '.txt', sep ='')))
		}

	}

	# Calculating and returning rate of success using the histogram function
	if( length(index$Crossings)){
		for ( day in daysAhead) {
			for ( i in c(5,10, 20) ) { #bins
				for ( model in c('Monte', 'Direct')){
					histTotal 	<- hist(timeseries[, 
						paste('Prob', model, day, sep = '')],
						seq(from=0, to=1, length.out=i+1) )
					succ 		<- which(timeseries$DiffEMA[-(1:day)] * 
						timeseries$DiffEMA[-((misc$datapoints - day + 1):
							misc$datapoints)] < 0)
					histSucc 	<- hist(timeseries[succ, 
						paste('Prob',model, day, sep ='')],
						seq(from=0, to=1, length.out=i+1))
					Succ 		<- as.data.frame(t(histSucc$counts))
					colnames(Succ) <- histSucc$mids
					Succ$Tick 	<- symbol
					Total 		<- as.data.frame(t(histTotal$counts))
					colnames(Total) <- histTotal$mids
					Total$Tick 	<- symbol
					write.table(Succ, file = 
							paste(folder, 'Succ', day, model, i,'.txt', sep = ''), 
						sep = ',', append = TRUE, row.names = FALSE, 
						col.names = !file.exists(
							paste(folder, 'Succ', day, model, i, '.txt', sep = '')))
					write.table(Total, file = 
							paste(folder, 'Total', day, model, i,'.txt', sep = ''), 
						sep = ',', append = TRUE, row.names = FALSE, 
						col.names = !file.exists(
							paste(folder, 'Total', day, model, i, '.txt', sep = '')))
				}
			}
		}
	}

	# Create price plot 
	temp <- stack(timeseries, c('Price', 'ShortEMA', 'LongEMA'))
	temp$x <- as.Date(rownames(timeseries))

	pl <- ggplot(temp, aes(x = x, y = values, color = ind) ) + geom_line() + 
		theme_bw() + xlab('') + ggtitle(paste('Price of', misc$Name )) + 
		ylab('Price')
	ggsave(paste(folder,symbol, '/Price.pdf', sep=''), plot = pl)


	#return(list(timeseries = timeseries, difftimeseries = diffTimeseries, 
	#	misc = misc, index =index))
	return(1)

}


ProbEst <- function(diffTimeseries, timeseries, misc) {
	probMonte 	<- matrix(NA, ncol = 5, nrow = misc$datapoints)
	probDirect		<- matrix(NA, ncol = 5, nrow = misc$datapoints)
	for (t in (misc$burn + 1):(misc$datapoints ) ) {
		probMonte[t, ] <- ProbEstMonte(timeseries, diffTimeseries, misc, t)
		probDirect[t, ] <- ProbEstDirect(timeseries, diffTimeseries, misc, t)
	}

	timeseries[, sapply(c(1, 2, 5, 10, 20), 
		function(x) paste('ProbMonte',x, sep=''))] <- probMonte
	timeseries[, sapply(c(1, 2, 5, 10, 20), 
		function(x) paste('ProbDirect',x, sep=''))] <- probDirect
	return(timeseries)
}



ProbEstMonte <- function(timeseries, diffTimeseries, misc, t){
	# Returns the probability for a sign change after 1, 2, 5, 10 and 20 days
	# using the rough estimate.
	temp <- matrix(rnorm(20 * misc$iterations / 2), ncol = 20, 
		nrow = misc$iterations / 2, byrow = TRUE)
	eps <- matrix(NA, ncol = 20, nrow = misc$iterations)
	eps[(1:(misc$iterations/2)) * 2 - 1, ] 	<- temp
	eps[(1:(misc$iterations/2)) * 2, ]		<- -temp

	X <- matrix(NA, ncol = 21, nrow = misc$iterations)

	X[, 1] <- diffTimeseries$ROI[t-1]
	sigma <- diffTimeseries$Sigma2[t-1]

	for ( i in 1:20) {
		sigma <- misc$a0 + misc$a1 * X[, i]^2 + misc$b1 * sigma
		X[, i + 1] <- sqrt(sigma) * eps[, i]
	}
	Q <- matrix(NA, ncol = 20, nrow = misc$iterations)
	X <- X[, -1]
	X <- t(apply(1 + X, 1, cumprod))
	P <- c(coredata(timeseries$Price[t])) * X
	Y <- misc$alpha * P[, 1] + (1 - misc$alpha) * 
		c(coredata(timeseries$ShortEMA[t]))
	Z <- misc$beta * P[, 1] + (1 - misc$beta) * 
		c(coredata(timeseries$LongEMA[t]))
	Q[ , 1] <- Y - Z
	for (i in 2:20 ) {
		Y <- misc$alpha * P[, i] + (1 - misc$alpha) * Y
		Z <- misc$beta * P[, i] + (1 - misc$beta) * Z
		Q[, i] <- Y - Z
	}
	cross <- Q[, c(1, 2, 5, 10, 20)] * c(coredata(timeseries$DiffEMA[t]))
	nr_crossings <- colSums(cross <= 0)

	return( nr_crossings / misc$iterations )
}


ProbEstDirect <- function(timeseries, diffTimeseries, misc, t){
	genEtaSolve <- function(t,Y0,Z0,P0,alpha,beta){
		fun1 <- function(c) { ( 1 - c ) ^ t }
		fun2 <- function(c, eta) {
			c * eta * (fun1(c) - eta^t ) / ( c + eta - 1 )
		}
		fun <- function(eta) {
			fun1(alpha) * Y0 - fun1(beta) * Z0  + (fun2(beta, eta) - 
				fun2(alpha, eta)) * P0
		}
		return(fun)
	}

	Y0 <- c(coredata(timeseries$ShortEMA[t]))
	Z0 <- c(coredata(timeseries$LongEMA[t]))
	P0 <- c(coredata(timeseries$Price[t]))

	daysAhead <- c(1, 2, 5, 10, 20)
	eta <- sapply(daysAhead, 
		function(x){ 
			try(uniroot(genEtaSolve(x, Y0, Z0, P0, misc$alpha, misc$beta), 
				c(1e-2,3))$root, silent = TRUE)
		}
	)
	eta <- suppressWarnings(as.numeric(eta))
	prob <- pnorm(eta^daysAhead, mean = 1, 
		sd = sqrt(daysAhead * diffTimeseries$Sigma2[t-1]))
	prob[is.na(prob)] <- 0
	if( Y0 - Z0 < 0 ) { 
		prob <- 1 - prob
	}
	return(prob)
}

