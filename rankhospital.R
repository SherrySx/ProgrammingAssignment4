rankhospital <- function (state, outcome, num="best") {
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
  
  ##get column index for the outcome
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
  ocdata_state <- ocdata[which(ocdata[,7]==state),c(2,colind)] 
  #sort by the outcome column ascending, and hopsital name
  ocdata_state <- ocdata_state[order(ocdata_state[,2], ocdata_state[,1], na.last=NA),]
  
  if (num == "best"){
    ocdata_state[1,1]
  }else if (num=="worst"){
    tail(ocdata_state, 1)[,1]
  }else if (!is.na(as.integer(num))){
    if (as.integer(num)> nrow(ocdata_state)){
      NA
    }else {
      ocdata_state[as.integer(num), 1]
    }
  }
}
