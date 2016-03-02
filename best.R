best <- function (state, outcome) {
  #turn warning off
  ocdata <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## convert character into number for outcomes
  for(i in c(11, 17, 23)){
    suppressWarnings(ocdata[,i]<-as.numeric(ocdata[,i]))
  }
  ## check state and outcomes are valid
  if(!state %in% unique(ocdata$State)){
    stop("invalid state")
  }
  ## get the column index for the outcome
  colind <- NULL
  if(tolower(outcome) == "heart attack"){
    colind <-11
  }else if(tolower(outcome) == "heart failure"){
    colind <-17
  }else if(tolower(outcome) == "pneumonia"){
    colind <-23
  }else{
    stop("invalid outcome")
  }
  #get the set belong to the state
  ocdata_state <- ocdata[which(ocdata[,7]==state),]
  #for that outcome (the column), get the min value, get the rows which have this value
  minnum <- min(ocdata_state[,colind], na.rm = TRUE)
  rows<- which(ocdata_state[,colind] == minnum)
  lowdf <- ocdata_state[rows,c(7, 2, colind)]
  #order by 1st -state column, but return the 2nd col value
  lowdf[order(lowdf[,1]),][,2]
}

