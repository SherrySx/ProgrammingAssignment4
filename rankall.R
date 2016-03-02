rankall <- function(outcome, num = "best") {
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  #turn warning off
  ocdata <-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## convert character into number for outcomes
  for(i in c(11, 17, 23)){
    suppressWarnings(ocdata[,i]<-as.numeric(ocdata[,i]))
  }
  
  #check outcome
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
  #get the set containing hospital name, state, outcome
  ocdata_simple <- ocdata[,c(2,7,colind)] 
  #for each state, get the nth of lowest outcome
  ocdata_bystate<- split(ocdata_simple, ocdata$State)
  
  rankhop <- function(x){
    #sort by outcome-col3, then by hospitalname
    x <- x[order(x[,3], x[,1], na.last=NA),]
    
    if (num == "best"){
      x[1,1]
    }else if (num=="worst"){
      tail(x, 1)[,1]
    }else if (!is.na(as.integer(num))){
      if (as.integer(num)> nrow(x)){
        NA
      }else {
        x[as.integer(num), 1]
      }
    }
  }
  
  ranklst <- lapply(ocdata_bystate,rankhop)
  ranklst_df <- data.frame(unlist(ranklst))
  colnames(ranklst_df)<- c("State, Hospital")
  ranklst_df
  
}
