### function takes state as input and the type of outcome of interest
### returns hospital name with lowest 30 day death rate
best <- function(state, outcome) {
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
        data <- data[which(complete.cases(data)),]
        
        ## return hospital name in that state with lowest 30 day death rate
        ## hospital, state, outcome
        besthosp <- data[which(data$outcome == min(data$outcome, na.rm = TRUE)),]
        
        if(nrow(besthosp) < 0)
                stop("no information retrieved")
        else if(nrow(besthosp) == 1)
                return(besthosp$hospital[1])
        else if(nrow(besthosp) > 1)
        {
                # this means that there were more than one hospital had the same outcome 
                # then use the tie handling rule
                #  If there is a tie for the best hospital for a given outcome, then the hospital names should
                #  be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
                #  and “f” are tied for best, then hospital “b” should be returned).
                besthop <- besthosp[order(besthosp$hospital, na.last = TRUE),]
                return(besthosp$hospital[1])
        }
}