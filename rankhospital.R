rankhospital <- function(state, outcome, num = "best") {
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
        
        ## filter out only the required state information 
        data <- data[which(data$state == state),]
        
        ## remove the incomplete cases
        data <- data[which(complete.cases(data$outcome)),]
        
        ## return hostpital name in that state with the given rank
        ## 30-day death rate
        ## order the rows
        rankhosp <- data[with(data, order(data$outcomes, data$hospital)),]

        if(num == "best") {
                return(rankhosp$hospital[1])
        }
        else if(num == "worst") {
                return(rankhosp$hospital[nrow(rankhosp)])
        }
        else if(is.numeric(num)) {
                return(rankhosp$hospital[num])
        }
        else {
                stop("invalid num")
        }
}