rankall <- function(outcome, num = "best") {
        
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

        if ( (is.character(num) && !(num %in% c("best","worst")))
                && !is.numeric(num)) { 
                stop("invalid num")
        }
        
        ## For each state, find the hospital of the given rank
        
        m <- matrix(,0,2)
        colnames(m) <- c("hospital","state")

        for (state in unique(outcome.measures[[7]])){
                
                ## Filtered outcome based on state
                o <- outcome.measures[outcome.measures[[7]] %in% state,]
                
                ## Filter columns and remove the not available rows 
                o <- o[c(2,column)]
                o <- o[o[2]!="Not Available",]
                
                ## Adjust outcome column as numeric vector
                o[,2] <- sapply( o[,2], as.numeric)
                
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                if (is.character(num)) {
                    if (num == "best") {
                            hospital <- sort(o[which.min(o[,2]),1])[1]
                    } else {
                            hospital <- sort(o[which.max(o[,2]),1])[1]
                    }
                } else {
                        hospital <- o[order(o[,2],o[,1]),][num,1]
                } 
                m <- rbind(m,c(hospital,state))
        }
        m <- m[order(m[,2],m[,1]),]
        as.data.frame(m,stringsAsFactors = FALSE,row.names = m[,2])
}