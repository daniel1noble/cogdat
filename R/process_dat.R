
#' Converting a complex cognition dataset from horizontal format to vertical format
#' @param data The formatted cognition data frame containing the keys: ID, Date and var
#' @return data.frame Returns a formatted data frame in vertical format that is ready for analysis
#' @description
#' @author Daniel Noble - daniel.noble@unsw.edu.au
#' @ examples
#' data <- read.csv("CognitionIndividual.csv", header = FALSE, stringsAsFactors = FALSE)
#' data_proc <- processDat(data)
#' @export

processDat <- function(data){
	# extract row, column for the relevant variables in c(row, col) format to ensure proper formatting of data
		idDim    <-  c(grep("[Ii][Dd]", data[,1]),1)
		dateDim <- c(1,grep("[Dd]ate", t(data[1,])))
		varDim  <-  c(idDim[1], dateDim[2])

	# Extract total number of trials
		numtrials <- ncol(data) - dateDim[2]
	
	## Create a replicate set of rows for each lizard that totals the number of trials for the first set of columns
		           datFirst <- unique(data[(idDim[1]+1):nrow(data), 1:(dateDim[2]-1)])
		           datFirst <- datFirst[-2,] ## Seems to be necessary 
			      dat <- datFirst[rep(rownames(datFirst), each=numtrials),]
		rownames(dat) <- 1:nrow(dat)
		colnames(dat) <- data[idDim[1], 1:(varDim[2]-1)] # Problem here for column names because factors. Can easily get around by using stringsAsFactors on loading data but should fix this to make sure that factors are coerced to characters

	#Extract the Trial data	
		trialdat <- t(data[,varDim[2]:ncol(data)])
		nameVarAll <- unique(trialdat[1,])
		AllTrialDat <- nameVarAll[1:varDim[1]-1]
		IndTrialDat <- as.character(na.omit(nameVarAll[varDim[1]+1:length(nameVarAll)]))

		colNames <- c(AllTrialDat, IndTrialDat)
		vectors <- matrix(ncol = length(IndTrialDat) + length(AllTrialDat), nrow = nrow(dat))
		colnames(vectors) <- colNames

	for(i in 1:length(colnames(vectors))){
		datExt <- data.frame(trialdat[,trialdat[1,] == colnames(vectors)[i]])
		vec <- as.character(unlist(datExt[-1,]))
		vectors[,i] <- vec
	}

data_proc <- data.frame(dat,  vectors, stringsAsFactors = FALSE)
return(data_proc)
}
