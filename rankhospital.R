rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcome.measures <<- read.csv("outcome-of-care-measures.csv", 
                                      colClasses = "character")

        ## Check if the state is valid
        if (!state %in% outcome.measures[[7]]) stop ("invalid state") 

        ## List of lower mortability outcome 
        outcome = gsub(" ",".",outcome)
        regex <- paste("^hospital.*mortality.*",outcome,"$",sep="")

        ## Column identification based on the desired outcome
        column <- grep(regex,colnames(outcome.measures),ignore.case = TRUE)

        ## Check if the outcome is valid
        if (!length(column) > 0) stop ("invalid outcome") 

        ## Filtered outcome based on state
        outcome.measures <- outcome.measures[outcome.measures[[7]] %in% state,]

        ## Filter columns and remove the not available rows 
        o <- outcome.measures[c(2,column)]
        o <- o[o[2]!="Not Available",]
        
        ## Adjust outcome column as numeric vector
        o[,2] <- sapply( o[,2], as.numeric)

        ## Return hospital name in that state with lowest 30-day death rate  
        if (is.character(num)) {
            if (num == "best") {
                    sort(o[which.min(o[,2]),1])[1]
            } else if (num == "worst") {
                    sort(o[which.max(o[,2]),1])[1]
            } else{
                    stop("invalid num")
            }
        } else if (is.numeric(num)) {
                o[order(o[,2],o[,1]),][num,1]
        } else{
                stop("invalid num")                
        }
        
}