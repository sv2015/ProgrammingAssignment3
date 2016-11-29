rankall <- function(outcome, num = "best") {
        ## named vector
        ocs <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) 
        
        ## read the outcome data
        data <- read.csv("rpogdata/outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors = FALSE)
        
        ## check that state and outcome are valid
        ## this only works if there is a guarantee that all states are represented in the data. If a valid state is not 
        ## included in the dataset then checking against it is checking against incomplete data so CA might not be in the data
        ## set but is a valid state
        if(!any(toupper(state) == toupper(data$State))) 
        {
                stop("invalid state")
        }
        if(!any(tolower(names(ocs)) == tolower(outcome))) 
        {
                stop("invalid outcome")
        }
        
        
        ## filter data
        # remove unwanted columns
        data <- subset(data, select = c(2, 7, ocs[which(names(ocs)==outcome)]))
        names(data) <- c("hospital", "state", "outcomes")        
        ## remove incomplete cases of outcomes
        data <- data[which(complete.cases(data$outcomes)==TRUE),]
        
        ## return hospital in each state with specified num
        data <- data[with(data, order(data$state, data$outcomes, data$hospital)),]
        statelist <- split(data, data$state)
        
        rankall <- unlist(lapply(statelist, hospitalname, num))

        rankall <- data.frame(rankall, names(rankall), row.names = names(rankall), stringsAsFactors = FALSE)
        names(rankall) <- c("hospital", "state")
        rankall
}

hospitalname <- function(d, num) {
        if(!is.numeric(num)) {
                if(num== "best") num <- 1
                else if(num == "worst") num <- nrow(d)
                else stop("invalid num")
        }        
        
        d$hospital[num]
}